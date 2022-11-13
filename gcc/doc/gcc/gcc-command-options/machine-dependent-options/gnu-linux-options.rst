..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: GNU/Linux

.. _gnu-linux-options:

GNU/Linux Options
^^^^^^^^^^^^^^^^^

These :samp:`-m` options are defined for GNU/Linux targets:

.. option:: -mglibc

  Use the GNU C library.  This is the default except
  on :samp:`*-*-linux-*uclibc*`, :samp:`*-*-linux-*musl*` and
  :samp:`*-*-linux-*android*` targets.

.. option:: -muclibc

  Use uClibc C library.  This is the default on
  :samp:`*-*-linux-*uclibc*` targets.

.. option:: -mmusl

  Use the musl C library.  This is the default on
  :samp:`*-*-linux-*musl*` targets.

.. option:: -mbionic

  Use Bionic C library.  This is the default on
  :samp:`*-*-linux-*android*` targets.

.. option:: -mandroid

  Compile code compatible with Android platform.  This is the default on
  :samp:`*-*-linux-*android*` targets.

  When compiling, this option enables :option:`-mbionic`, :option:`-fPIC`,
  :option:`-fno-exceptions` and :option:`-fno-rtti` by default.  When linking,
  this option makes the GCC driver pass Android-specific options to the linker.
  Finally, this option causes the preprocessor macro ``__ANDROID__``
  to be defined.

.. option:: -tno-android-cc

  Disable compilation effects of :option:`-mandroid`, i.e., do not enable
  :option:`-mbionic`, :option:`-fPIC`, :option:`-fno-exceptions` and
  :option:`-fno-rtti` by default.

.. option:: -tno-android-ld

  Disable linking effects of :option:`-mandroid`, i.e., pass standard Linux
  linking options to the linker.