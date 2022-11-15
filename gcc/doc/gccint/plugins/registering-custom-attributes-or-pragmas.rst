..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-attr:

Registering custom attributes or pragmas
****************************************

For analysis (or other) purposes it is useful to be able to add custom
attributes or pragmas.

The ``PLUGIN_ATTRIBUTES`` callback is called during attribute
registration. Use the ``register_attribute`` function to register
custom attributes.

.. code-block:: c++

  /* Attribute handler callback */
  static tree
  handle_user_attribute (tree *node, tree name, tree args,
                         int flags, bool *no_add_attrs)
  {
    return NULL_TREE;
  }

  /* Attribute definition */
  static struct attribute_spec user_attr =
    { "user", 1, 1, false,  false, false, false, handle_user_attribute, NULL };

  /* Plugin callback called during attribute registration.
  Registered with register_callback (plugin_name, PLUGIN_ATTRIBUTES, register_attributes, NULL)
  */
  static void
  register_attributes (void *event_data, void *data)
  {
    warning (0, G_("Callback to register attributes"));
    register_attribute (&user_attr);
  }

The PLUGIN_PRAGMAS callback is called once during pragmas
registration. Use the ``c_register_pragma``,
``c_register_pragma_with_data``,
``c_register_pragma_with_expansion``,
``c_register_pragma_with_expansion_and_data`` functions to register
custom pragmas and their handlers (which often want to call
``pragma_lex``) from :samp:`c-family/c-pragma.h`.

.. code-block:: c++

  /* Plugin callback called during pragmas registration. Registered with
       register_callback (plugin_name, PLUGIN_PRAGMAS,
                          register_my_pragma, NULL);
  */
  static void
  register_my_pragma (void *event_data, void *data)
  {
    warning (0, G_("Callback to register pragmas"));
    c_register_pragma ("GCCPLUGIN", "sayhello", handle_pragma_sayhello);
  }

It is suggested to pass ``"GCCPLUGIN"`` (or a short name identifying
your plugin) as the 'space' argument of your pragma.

Pragmas registered with ``c_register_pragma_with_expansion`` or
``c_register_pragma_with_expansion_and_data`` support
preprocessor expansions. For example:

.. code-block:: c++

  #define NUMBER 10
  #pragma GCCPLUGIN foothreshold (NUMBER)
