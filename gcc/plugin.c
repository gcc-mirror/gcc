/* Support for GCC plugin mechanism.
   Copyright (C) 2009 Free Software Foundation, Inc.

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

/* If plugin support is not enabled, do not try to execute any code
   that may reference libdl.  The generic code is still compiled in to
   avoid including too many conditional compilation paths in the rest
   of the compiler.  */
#ifdef ENABLE_PLUGIN
#include <dlfcn.h>
#endif

#include "coretypes.h"
#include "toplev.h"
#include "tree.h"
#include "tree-pass.h"
#include "intl.h"
#include "plugin.h"
#include "timevar.h"
#include "ggc.h"

#ifdef ENABLE_PLUGIN
#include "plugin-version.h"
#endif

/* Event names as strings.  Keep in sync with enum plugin_event.  */
const char *plugin_event_name[] =
{
  "PLUGIN_PASS_MANAGER_SETUP",
  "PLUGIN_FINISH_TYPE",
  "PLUGIN_FINISH_UNIT",
  "PLUGIN_CXX_CP_PRE_GENERICIZE",
  "PLUGIN_FINISH",
  "PLUGIN_INFO",
  "PLUGIN_GGC_START",
  "PLUGIN_GGC_MARKING",
  "PLUGIN_GGC_END",
  "PLUGIN_REGISTER_GGC_ROOTS",
  "PLUGIN_START_UNIT", 
  "PLUGIN_EVENT_LAST"
};

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
static struct callback_info *plugin_callbacks[PLUGIN_EVENT_LAST] = { NULL };

/* List node for an inserted pass instance. We need to keep track of all
   the newly-added pass instances (with 'added_pass_nodes' defined below)
   so that we can register their dump files after pass-positioning is finished.
   Registering dumping files needs to be post-processed or the
   static_pass_number of the opt_pass object would be modified and mess up
   the dump file names of future pass instances to be added.  */
struct pass_list_node
{
  struct opt_pass *pass;
  struct pass_list_node *next;
};

static struct pass_list_node *added_pass_nodes = NULL;
static struct pass_list_node *prev_added_pass_node;

#ifdef ENABLE_PLUGIN
/* Each plugin should define an initialization function with exactly
   this name.  */
static const char *str_plugin_init_func_name = "plugin_init";

/* Each plugin should define this symbol to assert that it is
   distributed under a GPL-compatible license.  */
static const char *str_license = "plugin_is_GPL_compatible";
#endif

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

  /* Then get rid of '.so' part of the name.  */
  strip_off_ending (base_name, strlen (base_name));

  return base_name;
}


/* Create a plugin_name_args object for the give plugin and insert it to
   the hash table. This function is called when -fplugin=/path/to/NAME.so
   option is processed.  */

void
add_new_plugin (const char* plugin_name)
{
  struct plugin_name_args *plugin;
  void **slot;
  char *base_name = get_plugin_base_name (plugin_name);

  /* If this is the first -fplugin= option we encounter, create 
     'plugin_name_args_tab' hash table.  */
  if (!plugin_name_args_tab)
    plugin_name_args_tab = htab_create (10, htab_hash_string, htab_str_eq,
                                        NULL);

  slot = htab_find_slot (plugin_name_args_tab, base_name, INSERT);

  /* If the same plugin (name) has been specified earlier, either emit an
     error or a warning message depending on if they have identical full
     (path) names.  */
  if (*slot)
    {
      plugin = (struct plugin_name_args *) *slot;
      if (strcmp (plugin->full_name, plugin_name))
        error ("Plugin %s was specified with different paths:\n%s\n%s",
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
          if (key_parsed)
            {
              error ("Malformed option -fplugin-arg-%s (multiple '=' signs)",
		     arg);
              return;
            }
          key_len = len;
          len = 0;
          value_start = ptr + 1;
          key_parsed = true;
          continue;
        }
      else
        ++len;
    }

  if (!key_start)
    {
      error ("Malformed option -fplugin-arg-%s (missing -<key>[=<value>])",
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
      && ((slot = htab_find_slot (plugin_name_args_tab, name, NO_INSERT))
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
    error ("Plugin %s should be specified before -fplugin-arg-%s "
           "in the command line", name, arg);

  /* We don't need the plugin's name anymore. Just release it.  */
  XDELETEVEC (name);
}


/* Insert the plugin pass at the proper position. Return true if the pass 
   is successfully added.

   PLUGIN_PASS_INFO - new pass to be inserted
   PASS_LIST        - root of the pass list to insert the new pass to  */

static bool
position_pass (struct plugin_pass *plugin_pass_info,
               struct opt_pass **pass_list)
{
  struct opt_pass *pass = *pass_list, *prev_pass = NULL;
  bool success = false;

  for ( ; pass; prev_pass = pass, pass = pass->next)
    {
      /* Check if the current pass is of the same type as the new pass and
         matches the name and the instance number of the reference pass.  */
      if (pass->type == plugin_pass_info->pass->type
          && pass->name
          && !strcmp (pass->name, plugin_pass_info->reference_pass_name)
          && ((plugin_pass_info->ref_pass_instance_number == 0)
              || (plugin_pass_info->ref_pass_instance_number ==
                  pass->static_pass_number)
              || (plugin_pass_info->ref_pass_instance_number == 1
                  && pass->todo_flags_start & TODO_mark_first_instance)))
        {
          struct opt_pass *new_pass = plugin_pass_info->pass;
          struct pass_list_node *new_pass_node;

          /* The following code (if-statement) is adopted from next_pass_1.  */
          if (new_pass->static_pass_number)
            {
              new_pass = XNEW (struct opt_pass);
              memcpy (new_pass, plugin_pass_info->pass, sizeof (*new_pass));
              new_pass->next = NULL;

              new_pass->todo_flags_start &= ~TODO_mark_first_instance;

              plugin_pass_info->pass->static_pass_number -= 1;
              new_pass->static_pass_number =
                  -plugin_pass_info->pass->static_pass_number;
            }
          else
            {
              new_pass->todo_flags_start |= TODO_mark_first_instance;
              new_pass->static_pass_number = -1;
            }

          /* Insert the new pass instance based on the positioning op.  */
          switch (plugin_pass_info->pos_op)
            {
              case PASS_POS_INSERT_AFTER:
                new_pass->next = pass->next;
                pass->next = new_pass;

		/* Skip newly inserted pass to avoid repeated
		   insertions in the case where the new pass and the
		   existing one have the same name.  */
                pass = new_pass; 
                break;
              case PASS_POS_INSERT_BEFORE:
                new_pass->next = pass;
                if (prev_pass)
                  prev_pass->next = new_pass;
                else
                  *pass_list = new_pass;
                break;
              case PASS_POS_REPLACE:
                new_pass->next = pass->next;
                if (prev_pass)
                  prev_pass->next = new_pass;
                else
                  *pass_list = new_pass;
                new_pass->sub = pass->sub;
                new_pass->tv_id = pass->tv_id;
                pass = new_pass;
                break;
              default:
                error ("Invalid pass positioning operation");
                return false;
            }

          /* Save the newly added pass (instance) in the added_pass_nodes
             list so that we can register its dump file later. Note that
             we cannot register the dump file now because doing so will modify
             the static_pass_number of the opt_pass object and therefore
             mess up the dump file name of future instances.  */
          new_pass_node = XCNEW (struct pass_list_node);
          new_pass_node->pass = new_pass;
          if (!added_pass_nodes)
            added_pass_nodes = new_pass_node;
          else
            prev_added_pass_node->next = new_pass_node;
          prev_added_pass_node = new_pass_node;

          success = true;
        }

      if (pass->sub && position_pass (plugin_pass_info, &pass->sub))
        success = true;
    }

  return success;
}


/* Hook into the pass lists (trees) a new pass registered by a plugin.

   PLUGIN_NAME - display name for the plugin
   PASS_INFO   - plugin pass information that specifies the opt_pass object,
                 reference pass, instance number, and how to position
                 the pass  */

static void
register_pass (const char *plugin_name, struct plugin_pass *pass_info)
{
  if (!pass_info->pass)
    {
      error ("No pass specified when registering a new pass in plugin %s",
             plugin_name);
      return;
    }

  if (!pass_info->reference_pass_name)
    {
      error ("No reference pass specified for positioning the pass "
             " from plugin %s", plugin_name);
      return;
    }

  /* Try to insert the new pass to the pass lists. We need to check all
     three lists as the reference pass could be in one (or all) of them.  */
  if (!position_pass (pass_info, &all_lowering_passes)
      && !position_pass (pass_info, &all_ipa_passes)
      && !position_pass (pass_info, &all_passes))
    error ("Failed to position pass %s registered by plugin %s. "
           "Cannot find the (specified instance of) reference pass %s",
           pass_info->pass->name, plugin_name, pass_info->reference_pass_name);
  else
    {
      /* OK, we have successfully inserted the new pass. We need to register
         the dump files for the newly added pass and its duplicates (if any).
         Because the registration of plugin passes happens after the
         command-line options are parsed, the options that specify single
         pass dumping (e.g. -fdump-tree-PASSNAME) cannot be used for new
         plugin passes. Therefore we currently can only enable dumping of
         new plugin passes when the 'dump-all' flags (e.g. -fdump-tree-all)
         are specified. While doing so, we also delete the pass_list_node
         objects created during pass positioning.  */
      while (added_pass_nodes)
        {
          struct pass_list_node *next_node = added_pass_nodes->next;
          enum tree_dump_index tdi;
          register_one_dump_file (added_pass_nodes->pass);
          if (added_pass_nodes->pass->type == SIMPLE_IPA_PASS
              || added_pass_nodes->pass->type == IPA_PASS)
            tdi = TDI_ipa_all;
          else if (added_pass_nodes->pass->type == GIMPLE_PASS)
            tdi = TDI_tree_all;
          else
            tdi = TDI_rtl_all;
          /* Check if dump-all flag is specified.  */
          if (get_dump_file_info (tdi)->state)
            get_dump_file_info (added_pass_nodes->pass->static_pass_number)
                ->state = get_dump_file_info (tdi)->state;
          XDELETE (added_pass_nodes);
          added_pass_nodes = next_node;
        }
    }
}


/* Register additional plugin information. NAME is the name passed to
   plugin_init. INFO is the information that should be registered. */

static void
register_plugin_info (const char* name, struct plugin_info *info)
{
  void **slot = htab_find_slot (plugin_name_args_tab, name, NO_INSERT);
  struct plugin_name_args *plugin = (struct plugin_name_args *) *slot;
  plugin->version = info->version;
  plugin->help = info->help;
}

/* Called from the plugin's initialization code. Register a single callback.
   This function can be called multiple times.

   PLUGIN_NAME - display name for this plugin
   EVENT       - which event the callback is for
   CALLBACK    - the callback to be called at the event
   USER_DATA   - plugin-provided data   */

void
register_callback (const char *plugin_name,
                   enum plugin_event event,
                   plugin_callback_func callback,
                   void *user_data)
{
  switch (event)
    {
      case PLUGIN_PASS_MANAGER_SETUP:
	gcc_assert (!callback);
        register_pass (plugin_name, (struct plugin_pass *) user_data);
        break;
      case PLUGIN_INFO:
	gcc_assert (!callback);
	register_plugin_info (plugin_name, (struct plugin_info *) user_data);
	break;
      case PLUGIN_REGISTER_GGC_ROOTS:
	gcc_assert (!callback);
        ggc_register_root_tab ((const struct ggc_root_tab*) user_data);
	break;
      case PLUGIN_FINISH_TYPE:
      case PLUGIN_START_UNIT:
      case PLUGIN_FINISH_UNIT:
      case PLUGIN_CXX_CP_PRE_GENERICIZE:
      case PLUGIN_GGC_START:
      case PLUGIN_GGC_MARKING:
      case PLUGIN_GGC_END:
      case PLUGIN_ATTRIBUTES:
      case PLUGIN_FINISH:
        {
          struct callback_info *new_callback;
          if (!callback)
            {
              error ("Plugin %s registered a null callback function "
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
      case PLUGIN_EVENT_LAST:
      default:
        error ("Unkown callback event registered by plugin %s",
               plugin_name);
    }
}


/* Called from inside GCC.  Invoke all plug-in callbacks registered with
   the specified event.

   EVENT    - the event identifier
   GCC_DATA - event-specific data provided by the compiler  */

void
invoke_plugin_callbacks (enum plugin_event event, void *gcc_data)
{
  timevar_push (TV_PLUGIN_RUN);

  switch (event)
    {
      case PLUGIN_FINISH_TYPE:
      case PLUGIN_START_UNIT:
      case PLUGIN_FINISH_UNIT:
      case PLUGIN_CXX_CP_PRE_GENERICIZE:
      case PLUGIN_ATTRIBUTES:
      case PLUGIN_FINISH:
      case PLUGIN_GGC_START:
      case PLUGIN_GGC_MARKING:
      case PLUGIN_GGC_END:
        {
          /* Iterate over every callback registered with this event and
             call it.  */
          struct callback_info *callback = plugin_callbacks[event];
          for ( ; callback; callback = callback->next)
            (*callback->func) (gcc_data, callback->user_data);
        }
        break;

      case PLUGIN_PASS_MANAGER_SETUP:
      case PLUGIN_EVENT_LAST:
      case PLUGIN_REGISTER_GGC_ROOTS:
      default:
        gcc_assert (false);
    }

  timevar_pop (TV_PLUGIN_RUN);
}

#ifdef ENABLE_PLUGIN
/* We need a union to cast dlsym return value to a function pointer
   as ISO C forbids assignment between function pointer and 'void *'.
   Use explicit union instead of __extension__(<union_cast>) for
   portability.  */
#define PTR_UNION_TYPE(TOTYPE) union { void *_q; TOTYPE _nq; }
#define PTR_UNION_AS_VOID_PTR(NAME) (NAME._q)
#define PTR_UNION_AS_CAST_PTR(NAME) (NAME._nq)

/* Try to initialize PLUGIN. Return true if successful. */

static bool
try_init_one_plugin (struct plugin_name_args *plugin)
{
  void *dl_handle;
  plugin_init_func plugin_init;
  char *err;
  PTR_UNION_TYPE (plugin_init_func) plugin_init_union;

  /* We use RTLD_NOW to accelerate binding and detect any mismatch
     between the API expected by the plugin and the GCC API; we use
     RTLD_GLOBAL which is useful to plugins which themselves call
     dlopen.  */
  dl_handle = dlopen (plugin->full_name, RTLD_NOW | RTLD_GLOBAL);
  if (!dl_handle)
    {
      error ("Cannot load plugin %s\n%s", plugin->full_name, dlerror ());
      return false;
    }

  /* Clear any existing error.  */
  dlerror ();

  /* Check the plugin license.  */
  if (dlsym (dl_handle, str_license) == NULL)
    fatal_error ("plugin %s is not licensed under a GPL-compatible license\n"
		 "%s", plugin->full_name, dlerror ());

  PTR_UNION_AS_VOID_PTR (plugin_init_union) =
      dlsym (dl_handle, str_plugin_init_func_name);
  plugin_init = PTR_UNION_AS_CAST_PTR (plugin_init_union);

  if ((err = dlerror ()) != NULL)
    {
      error ("Cannot find %s in plugin %s\n%s", str_plugin_init_func_name,
             plugin->full_name, err);
      return false;
    }

  /* Call the plugin-provided initialization routine with the arguments.  */
  if ((*plugin_init) (plugin, &gcc_version))
    {
      error ("Fail to initialize plugin %s", plugin->full_name);
      return false;
    }

  return true;
}


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
      htab_remove_elt (plugin_name_args_tab, plugin->base_name);
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

  for (event = PLUGIN_PASS_MANAGER_SETUP; event < PLUGIN_EVENT_LAST; event++)
    if (plugin_callbacks[event])
      return true;

  return false;
}


/* Dump to FILE the names and associated events for all the active
   plugins.  */

void
dump_active_plugins (FILE *file)
{
  int event;

  if (!plugins_active_p ())
    return;

  fprintf (stderr, "Event\t\t\tPlugins\n");
  for (event = PLUGIN_PASS_MANAGER_SETUP; event < PLUGIN_EVENT_LAST; event++)
    if (plugin_callbacks[event])
      {
	struct callback_info *ci;

	fprintf (file, "%s\t", plugin_event_name[event]);

	for (ci = plugin_callbacks[event]; ci; ci = ci->next)
	  fprintf (file, "%s ", ci->plugin_name);

	fprintf (file, "\n");
      }
}


/* Dump active plugins to stderr.  */

void
debug_active_plugins (void)
{
  dump_active_plugins (stderr);
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
