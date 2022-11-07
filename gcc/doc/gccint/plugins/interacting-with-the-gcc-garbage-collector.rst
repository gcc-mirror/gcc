..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-gc:

Interacting with the GCC Garbage Collector
******************************************

Some plugins may want to be informed when GGC (the GCC Garbage
Collector) is running. They can register callbacks for the
``PLUGIN_GGC_START`` and ``PLUGIN_GGC_END`` events (for which
the callback is called with a null ``gcc_data``) to be notified of
the start or end of the GCC garbage collection.

Some plugins may need to have GGC mark additional data. This can be
done by registering a callback (called with a null ``gcc_data``)
for the ``PLUGIN_GGC_MARKING`` event. Such callbacks can call the
``ggc_set_mark`` routine, preferably through the ``ggc_mark`` macro
(and conversely, these routines should usually not be used in plugins
outside of the ``PLUGIN_GGC_MARKING`` event).  Plugins that wish to hold
weak references to gc data may also use this event to drop weak references when
the object is about to be collected.  The ``ggc_marked_p`` function can be
used to tell if an object is marked, or is about to  be collected.  The
``gt_clear_cache`` overloads which some types define may also be of use in
managing weak references.

Some plugins may need to add extra GGC root tables, e.g. to handle their own
``GTY`` -ed data. This can be done with the ``PLUGIN_REGISTER_GGC_ROOTS``
pseudo-event with a null callback and the extra root table (of type ``struct
ggc_root_tab*``) as ``user_data``.  Running the
``gengtype -p source-dirfile-listplugin*.c ...``
utility generates these extra root tables.

You should understand the details of memory management inside GCC
before using ``PLUGIN_GGC_MARKING`` or ``PLUGIN_REGISTER_GGC_ROOTS``.