/* Support for GCC plugin mechanism.
   Copyright (C) 2009-2020 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* This file contains the support for GCC plugin mechanism based on the
   APIs described in doc/plugin.texi.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "options.h"
#include "tree-pass.h"
#include "diagnostic-core.h"
#include "flags.h"
#include "intl.h"
#include "plugin.h"

#ifdef ENABLE_PLUGIN
#include "plugin-version.h"
#endif

#ifdef __MINGW32__
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#endif

#define GCC_PLUGIN_STRINGIFY0(X) #X
#define GCC_PLUGIN_STRINGIFY1(X) GCC_PLUGIN_STRINGIFY0 (X)

/* Event names as strings.  Keep in sync with enum plugin_event.  */
static const char *plugin_event_name_init[] =
{
# define DEFEVENT(NAME) GCC_PLUGIN_STRINGIFY1 (NAME),
# include "plugin.def"
# undef DEFEVENT
};

/* A printf format large enough for the largest event above.  */
#define FMT_FOR_PLUGIN_EVENT "%-32s"

const char **plugin_event_name = plugin_event_name_init;

/* Event hashtable helpers.  */

struct event_hasher : nofree_ptr_hash <const char *>
{
  static inline hashval_t hash (const char **);
  static inline bool equal (const char **, const char **);
};

/* Helper function for the event hash table that hashes the entry V.  */

inline hashval_t
event_hasher::hash (const char **v)
{
  return htab_hash_string (*v);
}

/* Helper function for the event hash table that compares the name of an
   existing entry (S1) with the given string (S2).  */

inline bool
event_hasher::equal (const char **s1, const char **s2)
{
  return !strcmp (*s1, *s2);
}

/* A hash table to map event names to the position of the names in the
   plugin_event_name table.  */
static hash_table<event_hasher> *event_tab;

/* Keep track of the limit of allocated events and space ready for
   allocating events.  */
static int event_last = PLUGIN_EVENT_FIRST_DYNAMIC;
static int event_horizon = PLUGIN_EVENT_FIRST_DYNAMIC;

/* Hash table for the plugin_name_args objects created during command-line
   parsing.  */
static htab_t plugin_name_args_tab = NULL;

/* List node for keeping track of plugin-registered callback.  */
struct callback_info
{
  const char *plugin_name;   /* Name of plugin that registers the callback.  */
  plugin_callback_func func; /* Callback to be called.  */
  void *user_data;           /* plugin-specified data.  */
  struct callback_info *next;
};

/* An array of lists of 'callback_info' objects indexed by the event id.  */
static struct callback_info *plugin_callbacks_init[PLUGIN_EVENT_FIRST_DYNAMIC];
static struct callback_info **plugin_callbacks = plugin_callbacks_init;

/* For invoke_plugin_callbacks(), see plugin.h.  */
bool flag_plugin_added = false;

#ifdef ENABLE_PLUGIN
/* Each plugin should define an initialization function with exactly
   this name.  */
static const char *str_plugin_init_func_name = "plugin_init";

/* Each plugin should define this symbol to assert that it is
   distributed under a GPL-compatible license.  */
static const char *str_license = "plugin_is_GPL_compatible";
#endif

/* Helper function for hashing the base_name of the plugin_name_args
   structure to be inserted into the hash table.  */

static hashval_t
htab_hash_plugin (const PTR p)
{
  const struct plugin_name_args *plugin = (const struct plugin_name_args *) p;
  return htab_hash_string (plugin->base_name);
 }

/* Helper function for the hash table that compares the base_name of the
   existing entry (S1) with the given string (S2).  */

static int
htab_str_eq (const void *s1, const void *s2)
{
  const struct plugin_name_args *plugin = (const struct plugin_name_args *) s1;
  return !strcmp (plugin->base_name, (const char *) s2);
}


/* Given a plugin's full-path name FULL_NAME, e.g. /pass/to/NAME.so,
   return NAME.  */

static char *
get_plugin_base_name (const char *full_name)
{
  /* First get the base name part of the full-path name, i.e. NAME.so.  */
  char *base_name = xstrdup (lbasename (full_name));

  /* Then get rid of the extension in the name, e.g., .so.  */
  strip_off_ending (base_name, strlen (base_name));

  return base_name;
}


/* Create a plugin_name_args object for the given plugin and insert it
   to the hash table. This function is called when
   -fplugin=/path/to/NAME.so or -fplugin=NAME option is processed.  */

void
add_new_plugin (const char* plugin_name)
{
  struct plugin_name_args *plugin;
  void **slot;
  char *base_name;
  bool name_is_short;
  const char *pc;

  flag_plugin_added = true;

  /* Replace short names by their full path when relevant.  */
  name_is_short  = !IS_ABSOLUTE_PATH (plugin_name);
  for (pc = plugin_name; name_is_short && *pc; pc++)
    if (*pc == '.' || IS_DIR_SEPARATOR (*pc))
      name_is_short = false;

  if (name_is_short)
    {
      base_name = CONST_CAST (char*, plugin_name);

#if defined(__MINGW32__)
      static const char plugin_ext[] = ".dll";
#elif defined(__APPLE__)
      /* Mac OS has two types of libraries: dynamic libraries (.dylib) and
         plugins (.bundle). Both can be used with dlopen()/dlsym() but the
         former cannot be linked at build time (i.e., with the -lfoo linker
         option). A GCC plugin is therefore probably a Mac OS plugin but their
         use seems to be quite rare and the .bundle extension is more of a
         recommendation rather than the rule. This raises the questions of how
         well they are supported by tools (e.g., libtool). So to avoid
         complications let's use the .dylib extension for now. In the future,
         if this proves to be an issue, we can always check for both
         extensions.  */
      static const char plugin_ext[] = ".dylib";
#else
      static const char plugin_ext[] = ".so";
#endif

      plugin_name = concat (default_plugin_dir_name (), "/",
			    plugin_name, plugin_ext, NULL);
      if (access (plugin_name, R_OK))
	fatal_error
	  (input_location,
	   "inaccessible plugin file %s expanded from short plugin name %s: %m",
	   plugin_name, base_name);
    }
  else
    base_name = get_plugin_base_name (plugin_name);

  /* If this is the first -fplugin= option we encounter, create
     'plugin_name_args_tab' hash table.  */
  if (!plugin_name_args_tab)
    plugin_name_args_tab = htab_create (10, htab_hash_plugin, htab_str_eq,
                                        NULL);

  slot = htab_find_slot_with_hash (plugin_name_args_tab, base_name,
				   htab_hash_string (base_name), INSERT);

  /* If the same plugin (name) has been specified earlier, either emit an
     error or a warning message depending on if they have identical full
     (path) names.  */
  if (*slot)
    {
      plugin = (struct plugin_name_args *) *slot;
      if (strcmp (plugin->full_name, plugin_name))
	error ("plugin %qs was specified with different paths: %qs and %qs",
               plugin->base_name, plugin->full_name, plugin_name);
      return;
    }

  plugin = XCNEW (struct plugin_name_args);
  plugin->base_name = base_name;
  plugin->full_name = plugin_name;

  *slot = plugin;
}


/* Parse the -fplugin-arg-<name>-<key>[=<value>] option and create a
   'plugin_argument' object for the parsed key-value pair. ARG is
   the <name>-<key>[=<value>] part of the option.  */

void
parse_plugin_arg_opt (const char *arg)
{
  size_t len = 0, name_len = 0, key_len = 0, value_len = 0;
  const char *ptr, *name_start = arg, *key_start = NULL, *value_start = NULL;
  char *name, *key, *value;
  void **slot;
  bool name_parsed = false, key_parsed = false;

  /* Iterate over the ARG string and identify the starting character position
     of 'name', 'key', and 'value' and their lengths.  */
  for (ptr = arg; *ptr; ++ptr)
    {
      /* Only the first '-' encountered is considered a separator between
         'name' and 'key'. All the subsequent '-'s are considered part of
         'key'. For example, given -fplugin-arg-foo-bar-primary-key=value,
         the plugin name is 'foo' and the key is 'bar-primary-key'.  */
      if (*ptr == '-' && !name_parsed)
        {
          name_len = len;
          len = 0;
          key_start = ptr + 1;
          name_parsed = true;
          continue;
        }
      else if (*ptr == '=')
        {
	  if (!key_parsed) 
	    {
	      key_len = len;
	      len = 0;
	      value_start = ptr + 1;
	      key_parsed = true;
	    }
          continue;
        }
      else
        ++len;
    }

  if (!key_start)
    {
      error ("malformed option %<-fplugin-arg-%s%>: "
	     "missing %<-<key>[=<value>]%>",
             arg);
      return;
    }

  /* If the option doesn't contain the 'value' part, LEN is the KEY_LEN.
     Otherwise, it is the VALUE_LEN.  */
  if (!value_start)
    key_len = len;
  else
    value_len = len;

  name = XNEWVEC (char, name_len + 1);
  strncpy (name, name_start, name_len);
  name[name_len] = '\0';

  /* Check if the named plugin has already been specified earlier in the
     command-line.  */
  if (plugin_name_args_tab
      && ((slot = htab_find_slot_with_hash (plugin_name_args_tab, name,
					    htab_hash_string (name), NO_INSERT))
          != NULL))
    {
      struct plugin_name_args *plugin = (struct plugin_name_args *) *slot;

      key = XNEWVEC (char, key_len + 1);
      strncpy (key, key_start, key_len);
      key[key_len] = '\0';
      if (value_start)
        {
          value = XNEWVEC (char, value_len + 1);
          strncpy (value, value_start, value_len);
          value[value_len] = '\0';
        }
      else
        value = NULL;

      /* Create a plugin_argument object for the parsed key-value pair.
         If there are already arguments for this plugin, we will need to
         adjust the argument array size by creating a new array and deleting
         the old one. If the performance ever becomes an issue, we can
         change the code by pre-allocating a larger array first.  */
      if (plugin->argc > 0)
        {
          struct plugin_argument *args = XNEWVEC (struct plugin_argument,
                                                  plugin->argc + 1);
          memcpy (args, plugin->argv,
                  sizeof (struct plugin_argument) * plugin->argc);
          XDELETEVEC (plugin->argv);
          plugin->argv = args;
          ++plugin->argc;
        }
      else
        {
          gcc_assert (plugin->argv == NULL);
          plugin->argv = XNEWVEC (struct plugin_argument, 1);
          plugin->argc = 1;
        }

      plugin->argv[plugin->argc - 1].key = key;
      plugin->argv[plugin->argc - 1].value = value;
    }
  else
    error ("plugin %s should be specified before %<-fplugin-arg-%s%> "
           "in the command line", name, arg);

  /* We don't need the plugin's name anymore. Just release it.  */
  XDELETEVEC (name);
}

/* Register additional plugin information. NAME is the name passed to
   plugin_init. INFO is the information that should be registered. */

static void
register_plugin_info (const char* name, struct plugin_info *info)
{
  void **slot = htab_find_slot_with_hash (plugin_name_args_tab, name,
					  htab_hash_string (name), NO_INSERT);
  struct plugin_name_args *plugin;

  if (slot == NULL)
    {
      error ("unable to register info for plugin %qs - plugin name not found",
	     name);
      return;
    }
  plugin = (struct plugin_name_args *) *slot;
  plugin->version = info->version;
  plugin->help = info->help;
}

/* Look up the event id for NAME.  If the name is not found, return -1
   if INSERT is NO_INSERT.  */

int
get_named_event_id (const char *name, enum insert_option insert)
{
  const char ***slot;

  if (!event_tab)
    {
      int i;

      event_tab = new hash_table<event_hasher> (150);
      for (i = 0; i < event_last; i++)
	{
	  slot = event_tab->find_slot (&plugin_event_name[i], INSERT);
	  gcc_assert (*slot == HTAB_EMPTY_ENTRY);
	  *slot = &plugin_event_name[i];
	}
    }
  slot = event_tab->find_slot (&name, insert);
  if (slot == NULL)
    return -1;
  if (*slot != HTAB_EMPTY_ENTRY)
    return *slot - &plugin_event_name[0];

  if (event_last >= event_horizon)
    {
      event_horizon = event_last * 2;
      if (plugin_event_name == plugin_event_name_init)
	{
	  plugin_event_name = XNEWVEC (const char *, event_horizon);
	  memcpy (plugin_event_name, plugin_event_name_init,
		  sizeof plugin_event_name_init);
	  plugin_callbacks = XNEWVEC (struct callback_info *, event_horizon);
	  memcpy (plugin_callbacks, plugin_callbacks_init,
		  sizeof plugin_callbacks_init);
	}
      else
	{
	  plugin_event_name
	    = XRESIZEVEC (const char *, plugin_event_name, event_horizon);
	  plugin_callbacks = XRESIZEVEC (struct callback_info *,
					 plugin_callbacks, event_horizon);
	}
      /* All the pointers in the hash table will need to be updated.  */
      delete event_tab;
      event_tab = NULL;
    }
  else
    *slot = &plugin_event_name[event_last];
  plugin_event_name[event_last] = name;
  return event_last++;
}

/* Called from the plugin's initialization code. Register a single callback.
   This function can be called multiple times.

   PLUGIN_NAME - display name for this plugin
   EVENT       - which event the callback is for
   CALLBACK    - the callback to be called at the event
   USER_DATA   - plugin-provided data   */

void
register_callback (const char *plugin_name,
		   int event,
                   plugin_callback_func callback,
                   void *user_data)
{
  switch (event)
    {
      case PLUGIN_PASS_MANAGER_SETUP:
	gcc_assert (!callback);
        register_pass ((struct register_pass_info *) user_data);
        break;
      case PLUGIN_INFO:
	gcc_assert (!callback);
	register_plugin_info (plugin_name, (struct plugin_info *) user_data);
	break;
      case PLUGIN_REGISTER_GGC_ROOTS:
	gcc_assert (!callback);
        ggc_register_root_tab ((const struct ggc_root_tab*) user_data);
	break;
      case PLUGIN_EVENT_FIRST_DYNAMIC:
      default:
	if (event < PLUGIN_EVENT_FIRST_DYNAMIC || event >= event_last)
	  {
	    error ("unknown callback event registered by plugin %s",
		   plugin_name);
	    return;
	  }
      /* Fall through.  */
      case PLUGIN_START_PARSE_FUNCTION:
      case PLUGIN_FINISH_PARSE_FUNCTION:
      case PLUGIN_FINISH_TYPE:
      case PLUGIN_FINISH_DECL:
      case PLUGIN_START_UNIT:
      case PLUGIN_FINISH_UNIT:
      case PLUGIN_PRE_GENERICIZE:
      case PLUGIN_GGC_START:
      case PLUGIN_GGC_MARKING:
      case PLUGIN_GGC_END:
      case PLUGIN_ATTRIBUTES:
      case PLUGIN_PRAGMAS:
      case PLUGIN_FINISH:
      case PLUGIN_ALL_PASSES_START:
      case PLUGIN_ALL_PASSES_END:
      case PLUGIN_ALL_IPA_PASSES_START:
      case PLUGIN_ALL_IPA_PASSES_END:
      case PLUGIN_OVERRIDE_GATE:
      case PLUGIN_PASS_EXECUTION:
      case PLUGIN_EARLY_GIMPLE_PASSES_START:
      case PLUGIN_EARLY_GIMPLE_PASSES_END:
      case PLUGIN_NEW_PASS:
      case PLUGIN_INCLUDE_FILE:
        {
          struct callback_info *new_callback;
          if (!callback)
            {
              error ("plugin %s registered a null callback function "
		     "for event %s", plugin_name, plugin_event_name[event]);
              return;
            }
          new_callback = XNEW (struct callback_info);
          new_callback->plugin_name = plugin_name;
          new_callback->func = callback;
          new_callback->user_data = user_data;
          new_callback->next = plugin_callbacks[event];
          plugin_callbacks[event] = new_callback;
        }
        break;
    }
}

/* Remove a callback for EVENT which has been registered with for a plugin
   PLUGIN_NAME.  Return PLUGEVT_SUCCESS if a matching callback was
   found & removed, PLUGEVT_NO_CALLBACK if the event does not have a matching
   callback, and PLUGEVT_NO_SUCH_EVENT if EVENT is invalid.  */
int
unregister_callback (const char *plugin_name, int event)
{
  struct callback_info *callback, **cbp;

  if (event >= event_last)
    return PLUGEVT_NO_SUCH_EVENT;

  for (cbp = &plugin_callbacks[event]; (callback = *cbp); cbp = &callback->next)
    if (strcmp (callback->plugin_name, plugin_name) == 0)
      {
	*cbp = callback->next;
	return PLUGEVT_SUCCESS;
      }
  return PLUGEVT_NO_CALLBACK;
}

/* Invoke all plugin callbacks registered with the specified event,
   called from invoke_plugin_callbacks().  */

int
invoke_plugin_callbacks_full (int event, void *gcc_data)
{
  int retval = PLUGEVT_SUCCESS;

  timevar_push (TV_PLUGIN_RUN);

  switch (event)
    {
      case PLUGIN_EVENT_FIRST_DYNAMIC:
      default:
	gcc_assert (event >= PLUGIN_EVENT_FIRST_DYNAMIC);
	gcc_assert (event < event_last);
      /* Fall through.  */
      case PLUGIN_START_PARSE_FUNCTION:
      case PLUGIN_FINISH_PARSE_FUNCTION:
      case PLUGIN_FINISH_TYPE:
      case PLUGIN_FINISH_DECL:
      case PLUGIN_START_UNIT:
      case PLUGIN_FINISH_UNIT:
      case PLUGIN_PRE_GENERICIZE:
      case PLUGIN_ATTRIBUTES:
      case PLUGIN_PRAGMAS:
      case PLUGIN_FINISH:
      case PLUGIN_GGC_START:
      case PLUGIN_GGC_MARKING:
      case PLUGIN_GGC_END:
      case PLUGIN_ALL_PASSES_START:
      case PLUGIN_ALL_PASSES_END:
      case PLUGIN_ALL_IPA_PASSES_START:
      case PLUGIN_ALL_IPA_PASSES_END:
      case PLUGIN_OVERRIDE_GATE:
      case PLUGIN_PASS_EXECUTION:
      case PLUGIN_EARLY_GIMPLE_PASSES_START:
      case PLUGIN_EARLY_GIMPLE_PASSES_END:
      case PLUGIN_NEW_PASS:
      case PLUGIN_INCLUDE_FILE:
        {
          /* Iterate over every callback registered with this event and
             call it.  */
          struct callback_info *callback = plugin_callbacks[event];

	  if (!callback)
	    retval = PLUGEVT_NO_CALLBACK;
          for ( ; callback; callback = callback->next)
            (*callback->func) (gcc_data, callback->user_data);
        }
        break;

      case PLUGIN_PASS_MANAGER_SETUP:
      case PLUGIN_REGISTER_GGC_ROOTS:
        gcc_assert (false);
    }

  timevar_pop (TV_PLUGIN_RUN);
  return retval;
}

#ifdef ENABLE_PLUGIN

/* Try to initialize PLUGIN. Return true if successful. */

#ifdef __MINGW32__

// Return a message string for last error or NULL if unknown. Must be freed
// with LocalFree().
static inline char *
win32_error_msg ()
{
  char *msg;
  return FormatMessageA (FORMAT_MESSAGE_ALLOCATE_BUFFER |
			 FORMAT_MESSAGE_FROM_SYSTEM |
			 FORMAT_MESSAGE_IGNORE_INSERTS |
			 FORMAT_MESSAGE_MAX_WIDTH_MASK,
			 0,
			 GetLastError (),
			 MAKELANGID (LANG_NEUTRAL, SUBLANG_DEFAULT),
			 (char*)&msg,
			 0,
			 0)
    ? msg
    : NULL;
}

static bool
try_init_one_plugin (struct plugin_name_args *plugin)
{
  HMODULE dl_handle;
  plugin_init_func plugin_init;

  dl_handle = LoadLibrary (plugin->full_name);
  if (!dl_handle)
    {
      char *err = win32_error_msg ();
      error ("cannot load plugin %s\n%s", plugin->full_name, err);
      LocalFree (err);
      return false;
    }

  /* Check the plugin license. Unlike the name suggests, GetProcAddress()
     can be used for both functions and variables.  */
  if (GetProcAddress (dl_handle, str_license) == NULL)
    {
      char *err = win32_error_msg ();
      fatal_error (input_location,
		   "plugin %s is not licensed under a GPL-compatible license\n"
		   "%s", plugin->full_name, err);
    }

  /* Unlike dlsym(), GetProcAddress() returns a pointer to a function so we
     can cast directly without union tricks.  */
  plugin_init = (plugin_init_func)
    GetProcAddress (dl_handle, str_plugin_init_func_name);

  if (plugin_init == NULL)
    {
      char *err = win32_error_msg ();
      FreeLibrary (dl_handle);
      error ("cannot find %s in plugin %s\n%s", str_plugin_init_func_name,
             plugin->full_name, err);
      LocalFree (err);
      return false;
    }

  /* Call the plugin-provided initialization routine with the arguments.  */
  if ((*plugin_init) (plugin, &gcc_version))
    {
      FreeLibrary (dl_handle);
      error ("fail to initialize plugin %s", plugin->full_name);
      return false;
    }
  /* Leak dl_handle on purpose to ensure the plugin is loaded for the
     entire run of the compiler. */
  return true;
}

#else // POSIX-like with dlopen()/dlsym().

/* We need a union to cast dlsym return value to a function pointer
   as ISO C forbids assignment between function pointer and 'void *'.
   Use explicit union instead of __extension__(<union_cast>) for
   portability.  */
#define PTR_UNION_TYPE(TOTYPE) union { void *_q; TOTYPE _nq; }
#define PTR_UNION_AS_VOID_PTR(NAME) (NAME._q)
#define PTR_UNION_AS_CAST_PTR(NAME) (NAME._nq)

static bool
try_init_one_plugin (struct plugin_name_args *plugin)
{
  void *dl_handle;
  plugin_init_func plugin_init;
  const char *err;
  PTR_UNION_TYPE (plugin_init_func) plugin_init_union;

  /* We use RTLD_NOW to accelerate binding and detect any mismatch
     between the API expected by the plugin and the GCC API; we use
     RTLD_GLOBAL which is useful to plugins which themselves call
     dlopen.  */
  dl_handle = dlopen (plugin->full_name, RTLD_NOW | RTLD_GLOBAL);
  if (!dl_handle)
    {
      error ("cannot load plugin %s: %s", plugin->full_name, dlerror ());
      return false;
    }

  /* Clear any existing error.  */
  dlerror ();

  /* Check the plugin license.  */
  if (dlsym (dl_handle, str_license) == NULL)
    fatal_error (input_location,
		 "plugin %s is not licensed under a GPL-compatible license"
		 " %s", plugin->full_name, dlerror ());

  PTR_UNION_AS_VOID_PTR (plugin_init_union)
    = dlsym (dl_handle, str_plugin_init_func_name);
  plugin_init = PTR_UNION_AS_CAST_PTR (plugin_init_union);

  if ((err = dlerror ()) != NULL)
    {
      dlclose(dl_handle);
      error ("cannot find %s in plugin %s: %s", str_plugin_init_func_name,
             plugin->full_name, err);
      return false;
    }

  /* Call the plugin-provided initialization routine with the arguments.  */
  if ((*plugin_init) (plugin, &gcc_version))
    {
      dlclose(dl_handle);
      error ("failed to initialize plugin %s", plugin->full_name);
      return false;
    }
  /* leak dl_handle on purpose to ensure the plugin is loaded for the
     entire run of the compiler. */
  return true;
}
#endif

/* Routine to dlopen and initialize one plugin. This function is passed to
   (and called by) the hash table traverse routine. Return 1 for the
   htab_traverse to continue scan, 0 to stop.

   SLOT - slot of the hash table element
   INFO - auxiliary pointer handed to hash table traverse routine
          (unused in this function)  */

static int
init_one_plugin (void **slot, void * ARG_UNUSED (info))
{
  struct plugin_name_args *plugin = (struct plugin_name_args *) *slot;
  bool ok = try_init_one_plugin (plugin);
  if (!ok)
    {
      htab_remove_elt_with_hash (plugin_name_args_tab, plugin->base_name,
				 htab_hash_string (plugin->base_name));
      XDELETE (plugin);
    }
  return 1;
}

#endif	/* ENABLE_PLUGIN  */

/* Main plugin initialization function.  Called from compile_file() in
   toplev.c.  */

void
initialize_plugins (void)
{
  /* If no plugin was specified in the command-line, simply return.  */
  if (!plugin_name_args_tab)
    return;

  timevar_push (TV_PLUGIN_INIT);

#ifdef ENABLE_PLUGIN
  /* Traverse and initialize each plugin specified in the command-line.  */
  htab_traverse_noresize (plugin_name_args_tab, init_one_plugin, NULL);
#endif

  timevar_pop (TV_PLUGIN_INIT);
}

/* Release memory used by one plugin. */

static int
finalize_one_plugin (void **slot, void * ARG_UNUSED (info))
{
  struct plugin_name_args *plugin = (struct plugin_name_args *) *slot;
  XDELETE (plugin);
  return 1;
}

/* Free memory allocated by the plugin system. */

void
finalize_plugins (void)
{
  if (!plugin_name_args_tab)
    return;

  /* We can now delete the plugin_name_args object as it will no longer
     be used. Note that base_name and argv fields (both of which were also
     dynamically allocated) are not freed as they could still be used by
     the plugin code.  */

  htab_traverse_noresize (plugin_name_args_tab, finalize_one_plugin, NULL);

  /* PLUGIN_NAME_ARGS_TAB is no longer needed, just delete it.  */
  htab_delete (plugin_name_args_tab);
  plugin_name_args_tab = NULL;
}

/* Used to pass options to htab_traverse callbacks. */

struct print_options
{
  FILE *file;
  const char *indent;
};

/* Print the version of one plugin. */

static int
print_version_one_plugin (void **slot, void *data)
{
  struct print_options *opt = (struct print_options *) data;
  struct plugin_name_args *plugin = (struct plugin_name_args *) *slot;
  const char *version = plugin->version ? plugin->version : "Unknown version.";

  fprintf (opt->file, " %s%s: %s\n", opt->indent, plugin->base_name, version);
  return 1;
}

/* Print the version of each plugin. */

void
print_plugins_versions (FILE *file, const char *indent)
{
  struct print_options opt;
  opt.file = file;
  opt.indent = indent;
  if (!plugin_name_args_tab || htab_elements (plugin_name_args_tab) == 0)
    return;

  fprintf (file, "%sVersions of loaded plugins:\n", indent);
  htab_traverse_noresize (plugin_name_args_tab, print_version_one_plugin, &opt);
}

/* Print help for one plugin. SLOT is the hash table slot. DATA is the
   argument to htab_traverse_noresize. */

static int
print_help_one_plugin (void **slot, void *data)
{
  struct print_options *opt = (struct print_options *) data;
  struct plugin_name_args *plugin = (struct plugin_name_args *) *slot;
  const char *help = plugin->help ? plugin->help : "No help available .";

  char *dup = xstrdup (help);
  char *p, *nl;
  fprintf (opt->file, " %s%s:\n", opt->indent, plugin->base_name);

  for (p = nl = dup; nl; p = nl)
    {
      nl = strchr (nl, '\n');
      if (nl)
	{
	  *nl = '\0';
	  nl++;
	}
      fprintf (opt->file, "   %s %s\n", opt->indent, p);
    }

  free (dup);
  return 1;
}

/* Print help for each plugin. The output goes to FILE and every line starts
   with INDENT. */

void
print_plugins_help (FILE *file, const char *indent)
{
  struct print_options opt;
  opt.file = file;
  opt.indent = indent;
  if (!plugin_name_args_tab || htab_elements (plugin_name_args_tab) == 0)
    return;

  fprintf (file, "%sHelp for the loaded plugins:\n", indent);
  htab_traverse_noresize (plugin_name_args_tab, print_help_one_plugin, &opt);
}


/* Return true if plugins have been loaded.  */

bool
plugins_active_p (void)
{
  int event;

  for (event = PLUGIN_PASS_MANAGER_SETUP; event < event_last; event++)
    if (plugin_callbacks[event])
      return true;

  return false;
}


/* Dump to FILE the names and associated events for all the active
   plugins.  */

DEBUG_FUNCTION void
dump_active_plugins (FILE *file)
{
  int event;

  if (!plugins_active_p ())
    return;

  fprintf (file, FMT_FOR_PLUGIN_EVENT " | %s\n", _("Event"), _("Plugins"));
  for (event = PLUGIN_PASS_MANAGER_SETUP; event < event_last; event++)
    if (plugin_callbacks[event])
      {
	struct callback_info *ci;

	fprintf (file, FMT_FOR_PLUGIN_EVENT " |", plugin_event_name[event]);

	for (ci = plugin_callbacks[event]; ci; ci = ci->next)
	  fprintf (file, " %s", ci->plugin_name);

	putc ('\n', file);
      }
}


/* Dump active plugins to stderr.  */

DEBUG_FUNCTION void
debug_active_plugins (void)
{
  dump_active_plugins (stderr);
}

/* Give a warning if plugins are present, before an ICE message asking
   to submit a bug report.  */

void
warn_if_plugins (void)
{
  if (plugins_active_p ())
    {
      fnotice (stderr, "*** WARNING *** there are active plugins, do not report"
	       " this as a bug unless you can reproduce it without enabling"
	       " any plugins.\n");
      dump_active_plugins (stderr);
    }

}

/* The default version check. Compares every field in VERSION. */

bool
plugin_default_version_check (struct plugin_gcc_version *gcc_version,
			      struct plugin_gcc_version *plugin_version)
{
  if (!gcc_version || !plugin_version)
    return false;

  if (strcmp (gcc_version->basever, plugin_version->basever))
    return false;
  if (strcmp (gcc_version->datestamp, plugin_version->datestamp))
    return false;
  if (strcmp (gcc_version->devphase, plugin_version->devphase))
    return false;
  if (strcmp (gcc_version->revision, plugin_version->revision))
    return false;
  if (strcmp (gcc_version->configuration_arguments,
	      plugin_version->configuration_arguments))
    return false;
  return true;
}


/* Return the current value of event_last, so that plugins which provide
   additional functionality for events for the benefit of high-level plugins
   know how many valid entries plugin_event_name holds.  */

int
get_event_last (void)
{
  return event_last;
}


/* Retrieve the default plugin directory.  The gcc driver should have passed
   it as -iplugindir <dir> to the cc1 program, and it is queriable through the
   -print-file-name=plugin option to gcc.  */
const char*
default_plugin_dir_name (void)
{
  if (!plugindir_string)
    fatal_error (input_location,
		 "%<-iplugindir%> <dir> option not passed from the gcc driver");
  return plugindir_string;
}
