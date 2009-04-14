/* Public header file for plugins to include.
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

#ifndef GCC_PLUGIN_H
#define GCC_PLUGIN_H

/* Event names.  Keep in sync with plugin_event_name[].  */
enum plugin_event
{
  PLUGIN_PASS_MANAGER_SETUP,    /* To hook into pass manager.  */
  PLUGIN_FINISH_TYPE,           /* After finishing parsing a type.  */
  PLUGIN_FINISH_UNIT,           /* Useful for summary processing.  */
  PLUGIN_CXX_CP_PRE_GENERICIZE, /* Allows to see low level AST in C++ FE.  */
  PLUGIN_FINISH,                /* Called before GCC exits.  */
  PLUGIN_EVENT_LAST             /* Dummy event used for indexing callback
                                   array.  */
};

extern const char *plugin_event_name[];

struct plugin_argument
{
  char *key;    /* key of the argument.  */
  char *value;  /* value is optional and can be NULL.  */
};

enum pass_positioning_ops
{
  PASS_POS_INSERT_AFTER,  /* Insert after the reference pass.  */
  PASS_POS_INSERT_BEFORE, /* Insert before the reference pass.  */
  PASS_POS_REPLACE        /* Replace the reference pass.  */
};

struct plugin_pass
{
  struct opt_pass *pass;            /* New pass provided by the plugin.  */
  const char *reference_pass_name;  /* Name of the reference pass for hooking
                                       up the new pass.  */
  int ref_pass_instance_number;     /* Insert the pass at the specified
                                       instance number of the reference pass.
                                       Do it for every instance if it is 0.  */
  enum pass_positioning_ops pos_op; /* how to insert the new pass.  */
};

/* Function type for the plugin initialization routine. Each plugin module
   should define this as an externally-visible function with name
   "plugin_init."

   PLUGIN_NAME - name of the plugin (useful for error reporting)
   ARGC        - the size of the ARGV array
   ARGV        - an array of key-value argument pair

   Returns 0 if initialization finishes successfully.  */

typedef int (*plugin_init_func) (const char *plugin_name,
                                 int argc, struct plugin_argument *argv);

/* Declaration for "plugin_init" function so that it doesn't need to be
   duplicated in every plugin.  */
extern int plugin_init (const char *, int, struct plugin_argument *);

/* Function type for a plugin callback routine.

   GCC_DATA  - event-specific data provided by GCC
   USER_DATA - plugin-specific data provided by the plugin  */

typedef void (*plugin_callback_func) (void *gcc_data, void *user_data);

/* Called from the plugin's initialization code. Register a single callback.
   This function can be called multiple times.

   PLUGIN_NAME - display name for this plugin
   EVENT       - which event the callback is for
   CALLBACK    - the callback to be called at the event
   USER_DATA   - plugin-provided data   */

extern void register_callback (const char *plugin_name,
                               enum plugin_event event,
                               plugin_callback_func callback,
                               void *user_data);

#endif /* GCC_PLUGIN_H */
