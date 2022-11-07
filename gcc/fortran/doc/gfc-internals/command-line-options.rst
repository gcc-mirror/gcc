..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _command-line-options:

Command-Line Options
********************

Command-line options for :command:`gfortran` involve four interrelated
pieces within the Fortran compiler code.

The relevant command-line flag is defined in :samp:`lang.opt`, according
to the documentation in :ref:`gccint:options`.  This is then processed by the overall GCC
machinery to create the code that enables :command:`gfortran` and
:command:`gcc` to recognize the option in the command-line arguments and
call the relevant handler function.

This generated code calls the ``gfc_handle_option`` code in
:samp:`options.cc` with an enumerator variable indicating which option is
to be processed, and the relevant integer or string values associated
with that option flag.  Typically, ``gfc_handle_option`` uses these
arguments to set global flags which record the option states.

The global flags that record the option states are stored in the
``gfc_option_t`` struct, which is defined in :samp:`gfortran.h`.
Before the options are processed, initial values for these flags are set
in ``gfc_init_option`` in :samp:`options.cc`; these become the default
values for the options.