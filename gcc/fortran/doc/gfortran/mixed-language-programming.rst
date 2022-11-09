..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Interoperability, Mixed-language programming

.. _mixed-language-programming:

Mixed-Language Programming
--------------------------

.. toctree::
  :maxdepth: 2

  interoperability-with-c
  gnu-fortran-compiler-directives
  non-fortran-main-program
  naming-and-argument-passing-conventions

This chapter is about mixed-language interoperability, but also
applies if you link Fortran code compiled by different compilers.  In
most cases, use of the C Binding features of the Fortran 2003 and
later standards is sufficient.

For example, it is possible to mix Fortran code with C++ code as well
as C, if you declare the interface functions as ``extern "C"`` on
the C++ side and ``BIND(C)`` on the Fortran side, and follow the
rules for interoperability with C.  Note that you cannot manipulate
C++ class objects in Fortran or vice versa except as opaque pointers.

You can use the :command:`gfortran` command to link both Fortran and
non-Fortran code into the same program, or you can use :command:`gcc`
or :command:`g++` if you also add an explicit :option:`-lgfortran` option
to link with the Fortran library.  If your main program is written in
C or some other language instead of Fortran, see
:ref:`non-fortran-main-program`, below.
