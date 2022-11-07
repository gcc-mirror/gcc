..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-recording:

Recording information about pass execution
******************************************

The event PLUGIN_PASS_EXECUTION passes the pointer to the executed pass
(the same as current_pass) as ``gcc_data`` to the callback.  You can also
inspect cfun to find out about which function this pass is executed for.
Note that this event will only be invoked if the gate check (if
applicable, modified by PLUGIN_OVERRIDE_GATE) succeeds.
You can use other hooks, like ``PLUGIN_ALL_PASSES_START``,
``PLUGIN_ALL_PASSES_END``, ``PLUGIN_ALL_IPA_PASSES_START``,
``PLUGIN_ALL_IPA_PASSES_END``, ``PLUGIN_EARLY_GIMPLE_PASSES_START``,
and/or ``PLUGIN_EARLY_GIMPLE_PASSES_END`` to manipulate global state
in your plugin(s) in order to get context for the pass execution.