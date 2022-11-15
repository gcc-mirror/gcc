..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugin-api:

Plugin API
**********

Plugins are activated by the compiler at specific events as defined in
:samp:`gcc-plugin.h`.  For each event of interest, the plugin should
call ``register_callback`` specifying the name of the event and
address of the callback function that will handle that event.

The header :samp:`gcc-plugin.h` must be the first gcc header to be included.

Plugin license check
^^^^^^^^^^^^^^^^^^^^

Every plugin should define the global symbol ``plugin_is_GPL_compatible``
to assert that it has been licensed under a GPL-compatible license.
If this symbol does not exist, the compiler will emit a fatal error
and exit with the error message:

.. code-block::

  fatal error: plugin name is not licensed under a GPL-compatible license
  name: undefined symbol: plugin_is_GPL_compatible
  compilation terminated

The declared type of the symbol should be int, to match a forward declaration
in :samp:`gcc-plugin.h` that suppresses C++ mangling.  It does not need to be in
any allocated section, though.  The compiler merely asserts that
the symbol exists in the global scope.  Something like this is enough:

.. code-block:: c++

  int plugin_is_GPL_compatible;

Plugin initialization
^^^^^^^^^^^^^^^^^^^^^

Every plugin should export a function called ``plugin_init`` that
is called right after the plugin is loaded. This function is
responsible for registering all the callbacks required by the plugin
and do any other required initialization.

This function is called from ``compile_file`` right before invoking
the parser.  The arguments to ``plugin_init`` are:

* ``plugin_info`` : Plugin invocation information.

* ``version`` : GCC version.

The ``plugin_info`` struct is defined as follows:

.. code-block:: c++

  struct plugin_name_args
  {
    char *base_name;              /* Short name of the plugin
                                     (filename without .so suffix). */
    const char *full_name;        /* Path to the plugin as specified with
                                     -fplugin=. */
    int argc;                     /* Number of arguments specified with
                                     -fplugin-arg-.... */
    struct plugin_argument *argv; /* Array of ARGC key-value pairs. */
    const char *version;          /* Version string provided by plugin. */
    const char *help;             /* Help string provided by plugin. */
  }

If initialization fails, ``plugin_init`` must return a non-zero
value.  Otherwise, it should return 0.

The version of the GCC compiler loading the plugin is described by the
following structure:

.. code-block:: c++

  struct plugin_gcc_version
  {
    const char *basever;
    const char *datestamp;
    const char *devphase;
    const char *revision;
    const char *configuration_arguments;
  };

The function ``plugin_default_version_check`` takes two pointers to
such structure and compare them field by field. It can be used by the
plugin's ``plugin_init`` function.

The version of GCC used to compile the plugin can be found in the symbol
``gcc_version`` defined in the header :samp:`plugin-version.h`. The
recommended version check to perform looks like

.. code-block:: c++

  #include "plugin-version.h"
  ...

  int
  plugin_init (struct plugin_name_args *plugin_info,
               struct plugin_gcc_version *version)
  {
    if (!plugin_default_version_check (version, &gcc_version))
      return 1;

  }

but you can also check the individual fields if you want a less strict check.

Plugin callbacks
^^^^^^^^^^^^^^^^

Callback functions have the following prototype:

.. code-block:: c++

  /* The prototype for a plugin callback function.
       gcc_data  - event-specific data provided by GCC
       user_data - plugin-specific data provided by the plug-in.  */
  typedef void (*plugin_callback_func)(void *gcc_data, void *user_data);

Callbacks can be invoked at the following pre-determined events:

.. code-block:: c++

  enum plugin_event
  {
    PLUGIN_START_PARSE_FUNCTION,  /* Called before parsing the body of a function. */
    PLUGIN_FINISH_PARSE_FUNCTION, /* After finishing parsing a function. */
    PLUGIN_PASS_MANAGER_SETUP,    /* To hook into pass manager.  */
    PLUGIN_FINISH_TYPE,           /* After finishing parsing a type.  */
    PLUGIN_FINISH_DECL,           /* After finishing parsing a declaration. */
    PLUGIN_FINISH_UNIT,           /* Useful for summary processing.  */
    PLUGIN_PRE_GENERICIZE,        /* Allows to see low level AST in C and C++ frontends.  */
    PLUGIN_FINISH,                /* Called before GCC exits.  */
    PLUGIN_INFO,                  /* Information about the plugin. */
    PLUGIN_GGC_START,             /* Called at start of GCC Garbage Collection. */
    PLUGIN_GGC_MARKING,           /* Extend the GGC marking. */
    PLUGIN_GGC_END,               /* Called at end of GGC. */
    PLUGIN_REGISTER_GGC_ROOTS,    /* Register an extra GGC root table. */
    PLUGIN_ATTRIBUTES,            /* Called during attribute registration */
    PLUGIN_START_UNIT,            /* Called before processing a translation unit.  */
    PLUGIN_PRAGMAS,               /* Called during pragma registration. */
    /* Called before first pass from all_passes.  */
    PLUGIN_ALL_PASSES_START,
    /* Called after last pass from all_passes.  */
    PLUGIN_ALL_PASSES_END,
    /* Called before first ipa pass.  */
    PLUGIN_ALL_IPA_PASSES_START,
    /* Called after last ipa pass.  */
    PLUGIN_ALL_IPA_PASSES_END,
    /* Allows to override pass gate decision for current_pass.  */
    PLUGIN_OVERRIDE_GATE,
    /* Called before executing a pass.  */
    PLUGIN_PASS_EXECUTION,
    /* Called before executing subpasses of a GIMPLE_PASS in
       execute_ipa_pass_list.  */
    PLUGIN_EARLY_GIMPLE_PASSES_START,
    /* Called after executing subpasses of a GIMPLE_PASS in
       execute_ipa_pass_list.  */
    PLUGIN_EARLY_GIMPLE_PASSES_END,
    /* Called when a pass is first instantiated.  */
    PLUGIN_NEW_PASS,
  /* Called when a file is #include-d or given via the #line directive.
     This could happen many times.  The event data is the included file path,
     as a const char* pointer.  */
    PLUGIN_INCLUDE_FILE,

    /* Called when -fanalyzer starts. The event data is an
       ana::plugin_analyzer_init_iface *.  */
    PLUGIN_ANALYZER_INIT,

    PLUGIN_EVENT_FIRST_DYNAMIC    /* Dummy event used for indexing callback
                                     array.  */
  };

In addition, plugins can also look up the enumerator of a named event,
and / or generate new events dynamically, by calling the function
``get_named_event_id``.

To register a callback, the plugin calls ``register_callback`` with
the arguments:

* ``char *name`` : Plugin name.

* ``int event`` : The event code.

* ``plugin_callback_func callback`` : The function that handles ``event``.

* ``void *user_data`` : Pointer to plugin-specific data.

For the PLUGIN_PASS_MANAGER_SETUP, PLUGIN_INFO, and
PLUGIN_REGISTER_GGC_ROOTS pseudo-events the ``callback`` should be null,
and the ``user_data`` is specific.

When the PLUGIN_PRAGMAS event is triggered (with a null pointer as
data from GCC), plugins may register their own pragmas.  Notice that
pragmas are not available from :samp:`lto1`, so plugins used with
``-flto`` option to GCC during link-time optimization cannot use
pragmas and do not even see functions like ``c_register_pragma`` or
``pragma_lex``.

The PLUGIN_INCLUDE_FILE event, with a ``const char*`` file path as
GCC data, is triggered for processing of ``#include`` or
``#line`` directives.

The PLUGIN_FINISH event is the last time that plugins can call GCC
functions, notably emit diagnostics with ``warning``, ``error``
etc.
