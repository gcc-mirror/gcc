..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: options, linking, linking, static

.. _linking:

Options for Linking
*******************

These options come into play when the compiler links object files into an
executable output file.  They are meaningless if the compiler is not doing
a link step.

.. option:: -defaultlib=libname

  .. index:: -defaultlib=

  Specify the library to use instead of libphobos when linking.  Options
  specifying the linkage of libphobos, such as :option:`-static-libphobos`
  or :option:`-shared-libphobos`, are ignored.

.. option:: -debuglib=libname

  .. index:: -debuglib=

  Specify the debug library to use instead of libphobos when linking.
  This option has no effect unless the :option:`-g` option was also given
  on the command line.  Options specifying the linkage of libphobos, such
  as :option:`-static-libphobos` or :option:`-shared-libphobos`, are ignored.

.. option:: -nophoboslib

  .. index:: -nophoboslib

  Do not use the Phobos or D runtime library when linking.  Options specifying
  the linkage of libphobos, such as :option:`-static-libphobos` or
  :option:`-shared-libphobos`, are ignored.  The standard system libraries are
  used normally, unless :option:`-nostdlib` or :option:`-nodefaultlibs` is used.

.. option:: -shared-libphobos

  .. index:: -shared-libphobos

  On systems that provide :samp:`libgphobos` and :samp:`libgdruntime` as a
  shared and a static library, this option forces the use of the shared
  version.  If no shared version was built when the compiler was configured,
  this option has no effect.

.. option:: -static-libphobos

  .. index:: -static-libphobos

  On systems that provide :samp:`libgphobos` and :samp:`libgdruntime` as a
  shared and a static library, this option forces the use of the static
  version.  If no static version was built when the compiler was configured,
  this option has no effect.
