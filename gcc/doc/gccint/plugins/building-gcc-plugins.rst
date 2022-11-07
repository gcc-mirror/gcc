..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _plugins-building:

Building GCC plugins
********************

If plugins are enabled, GCC installs the headers needed to build a
plugin (somewhere in the installation tree, e.g. under
:samp:`/usr/local`).  In particular a :samp:`plugin/include` directory
is installed, containing all the header files needed to build plugins.

On most systems, you can query this ``plugin`` directory by
invoking :command:`gcc -print-file-name=plugin` (replace if needed
:command:`gcc` with the appropriate program path).

Inside plugins, this ``plugin`` directory name can be queried by
calling ``default_plugin_dir_name ()``.

Plugins may know, when they are compiled, the GCC version for which
:samp:`plugin-version.h` is provided.  The constant macros
``GCCPLUGIN_VERSION_MAJOR``, ``GCCPLUGIN_VERSION_MINOR``,
``GCCPLUGIN_VERSION_PATCHLEVEL``, ``GCCPLUGIN_VERSION`` are
integer numbers, so a plugin could ensure it is built for GCC 4.7 with

.. code-block:: c++

  #if GCCPLUGIN_VERSION != 4007
  #error this GCC plugin is for GCC 4.7
  #endif

The following GNU Makefile excerpt shows how to build a simple plugin:

.. code-block::

  HOST_GCC=g++
  TARGET_GCC=gcc
  PLUGIN_SOURCE_FILES= plugin1.c plugin2.cc
  GCCPLUGINS_DIR:= $(shell $(TARGET_GCC) -print-file-name=plugin)
  CXXFLAGS+= -I$(GCCPLUGINS_DIR)/include -fPIC -fno-rtti -O2

  plugin.so: $(PLUGIN_SOURCE_FILES)
     $(HOST_GCC) -shared $(CXXFLAGS) $^ -o $@

A single source file plugin may be built with ``g++ -I`gcc
-print-file-name=plugin`/include -fPIC -shared -fno-rtti -O2 plugin.cc -o
plugin.so``, using backquote shell syntax to query the :samp:`plugin`
directory.

Plugin support on Windows/MinGW has a number of limitations and
additional requirements. When building a plugin on Windows we have to
link an import library for the corresponding backend executable, for
example, :samp:`cc1.exe`, :samp:`cc1plus.exe`, etc., in order to gain
access to the symbols provided by GCC. This means that on Windows a
plugin is language-specific, for example, for C, C++, etc. If you wish
to use your plugin with multiple languages, then you will need to
build multiple plugin libraries and either instruct your users on how
to load the correct version or provide a compiler wrapper that does
this automatically.

Additionally, on Windows the plugin library has to export the
``plugin_is_GPL_compatible`` and ``plugin_init`` symbols. If you
do not wish to modify the source code of your plugin, then you can use
the :option:`-Wl,--export-all-symbols` option or provide a suitable DEF
file. Alternatively, you can export just these two symbols by decorating
them with ``__declspec(dllexport)``, for example:

.. code-block:: c++

  #ifdef _WIN32
  __declspec(dllexport)
  #endif
  int plugin_is_GPL_compatible;

  #ifdef _WIN32
  __declspec(dllexport)
  #endif
  int plugin_init (plugin_name_args *, plugin_gcc_version *)

The import libraries are installed into the ``plugin`` directory
and their names are derived by appending the ``.a`` extension to
the backend executable names, for example, :samp:`cc1.exe.a`,
:samp:`cc1plus.exe.a`, etc. The following command line shows how to
build the single source file plugin on Windows to be used with the C++
compiler:

.. code-block::

  g++ -I`gcc -print-file-name=plugin`/include -shared -Wl,--export-all-symbols \
  -o plugin.dll plugin.cc `gcc -print-file-name=plugin`/cc1plus.exe.a

When a plugin needs to use :command:`gengtype`, be sure that both
:samp:`gengtype` and :samp:`gtype.state` have the same version as the
GCC for which the plugin is built.