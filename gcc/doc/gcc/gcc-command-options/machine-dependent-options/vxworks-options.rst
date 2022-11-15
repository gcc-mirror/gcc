..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. program:: VxWorks

.. index:: VxWorks Options

.. _vxworks-options:

VxWorks Options
^^^^^^^^^^^^^^^

The options in this section are defined for all VxWorks targets.
Options specific to the target hardware are listed with the other
options for that target.

.. option:: -mrtp

  GCC can generate code for both VxWorks kernels and real time processes
  (RTPs).  This option switches from the former to the latter.  It also
  defines the preprocessor macro ``__RTP__``.

.. option:: -non-static

  Link an RTP executable against shared libraries rather than static
  libraries.  The options :option:`-static` and :option:`-shared` can
  also be used for RTPs (see :ref:`link-options`); :option:`-static`
  is the default.

.. option:: -Bstatic, -Bdynamic

  These options are passed down to the linker.  They are defined for
  compatibility with Diab.

.. option:: -Xbind-lazy

  Enable lazy binding of function calls.  This option is equivalent to
  :option:`-Wl,-z,now` and is defined for compatibility with Diab.

.. option:: -Xbind-now

  Disable lazy binding of function calls.  This option is the default and
  is defined for compatibility with Diab.
