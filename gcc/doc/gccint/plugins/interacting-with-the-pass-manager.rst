..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-pass:

Interacting with the pass manager
*********************************

There needs to be a way to add/reorder/remove passes dynamically. This
is useful for both analysis plugins (plugging in after a certain pass
such as CFG or an IPA pass) and optimization plugins.

Basic support for inserting new passes or replacing existing passes is
provided. A plugin registers a new pass with GCC by calling
``register_callback`` with the ``PLUGIN_PASS_MANAGER_SETUP``
event and a pointer to a ``struct register_pass_info`` object defined as follows

.. code-block:: c++

  enum pass_positioning_ops
  {
    PASS_POS_INSERT_AFTER,  // Insert after the reference pass.
    PASS_POS_INSERT_BEFORE, // Insert before the reference pass.
    PASS_POS_REPLACE        // Replace the reference pass.
  };

  struct register_pass_info
  {
    struct opt_pass *pass;            /* New pass provided by the plugin.  */
    const char *reference_pass_name;  /* Name of the reference pass for hooking
                                         up the new pass.  */
    int ref_pass_instance_number;     /* Insert the pass at the specified
                                         instance number of the reference pass.  */
                                      /* Do it for every instance if it is 0.  */
    enum pass_positioning_ops pos_op; /* how to insert the new pass.  */
  };

  /* Sample plugin code that registers a new pass.  */
  int
  plugin_init (struct plugin_name_args *plugin_info,
               struct plugin_gcc_version *version)
  {
    struct register_pass_info pass_info;

    ...

    /* Code to fill in the pass_info object with new pass information.  */

    ...

    /* Register the new pass.  */
    register_callback (plugin_info->base_name, PLUGIN_PASS_MANAGER_SETUP, NULL, &pass_info);

    ...
  }
