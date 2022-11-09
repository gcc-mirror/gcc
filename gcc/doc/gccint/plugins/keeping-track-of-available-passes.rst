..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-tracking:

Keeping track of available passes
*********************************

When your plugin is loaded, you can inspect the various
pass lists to determine what passes are available.  However, other
plugins might add new passes.  Also, future changes to GCC might cause
generic passes to be added after plugin loading.
When a pass is first added to one of the pass lists, the event
``PLUGIN_NEW_PASS`` is invoked, with the callback parameter
``gcc_data`` pointing to the new pass.
