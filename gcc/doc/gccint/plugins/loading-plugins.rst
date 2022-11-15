..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-loading:

Loading Plugins
***************

Plugins are supported on platforms that support :option:`-ldl
-rdynamic` as well as Windows/MinGW. They are loaded by the compiler
using ``dlopen`` or equivalent and invoked at pre-determined
locations in the compilation process.

Plugins are loaded with

:option:`-fplugin=/path/to/name.ext` :option:`-fplugin-arg-name-key1[=value1]`

Where :samp:`{name}` is the plugin name and :samp:`{ext}` is the platform-specific
dynamic library extension. It should be ``dll`` on Windows/MinGW,
``dylib`` on Darwin/Mac OS X, and ``so`` on all other platforms.
The plugin arguments are parsed by GCC and passed to respective
plugins as key-value pairs. Multiple plugins can be invoked by
specifying multiple :option:`-fplugin` arguments.

A plugin can be simply given by its short name (no dots or
slashes). When simply passing :option:`-fplugin=name`, the plugin is
loaded from the :samp:`plugin` directory, so :option:`-fplugin=name` is
the same as :option:`-fplugin\=\`gcc -print-file-name=plugin\`/name.ext`,
using backquote shell syntax to query the :samp:`plugin` directory.
