..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-description:

Giving information about a plugin
*********************************

A plugin should give some information to the user about itself. This
uses the following structure:

.. code-block:: c++

  struct plugin_info
  {
    const char *version;
    const char *help;
  };

Such a structure is passed as the ``user_data`` by the plugin's
init routine using ``register_callback`` with the
``PLUGIN_INFO`` pseudo-event and a null callback.
