..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: options, linking, linking, static

.. _link-options:

Influencing the linking step
****************************

These options come into play when the compiler links object files into an
executable output file. They are meaningless if the compiler is not doing
a link step.

.. index:: static-libgfortran

.. option:: -static-libgfortran

  On systems that provide :samp:`libgfortran` as a shared and a static
  library, this option forces the use of the static version. If no
  shared version of :samp:`libgfortran` was built when the compiler was
  configured, this option has no effect.

.. index:: static-libquadmath

.. option:: -static-libquadmath

  On systems that provide :samp:`libquadmath` as a shared and a static
  library, this option forces the use of the static version. If no
  shared version of :samp:`libquadmath` was built when the compiler was
  configured, this option has no effect.

  Please note that the :samp:`libquadmath` runtime library is licensed under the
  GNU Lesser General Public License (LGPL), and linking it statically introduces
  requirements when redistributing the resulting binaries.
