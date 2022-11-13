..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: preprocessor options, options, preprocessor

.. _preprocessor-options:

Options Controlling the Preprocessor
************************************

These options control the C preprocessor, which is run on each C source
file before actual compilation.

If you use the :option:`-E` option, nothing is done except preprocessing.
Some of these options make sense only together with :option:`-E` because
they cause the preprocessor output to be unsuitable for actual
compilation.

In addition to the options listed here, there are a number of options
to control search paths for include files documented in
:ref:`directory-options`.
Options to control preprocessor diagnostics are listed in
:ref:`warning-options`.

.. include:: ../../../../doc/cppopts.rst


.. option:: -Wp,option

  You can use :option:`-Wp,option` to bypass the compiler driver
  and pass :samp:`{option}` directly through to the preprocessor.  If
  :samp:`{option}` contains commas, it is split into multiple options at the
  commas.  However, many options are modified, translated or interpreted
  by the compiler driver before being passed to the preprocessor, and
  :option:`-Wp` forcibly bypasses this phase.  The preprocessor's direct
  interface is undocumented and subject to change, so whenever possible
  you should avoid using :option:`-Wp` and let the driver handle the
  options instead.

.. option:: -Xpreprocessor {option}

  Pass :samp:`{option}` as an option to the preprocessor.  You can use this to
  supply system-specific preprocessor options that GCC does not
  recognize.

  If you want to pass an option that takes an argument, you must use
  :option:`-Xpreprocessor` twice, once for the option and once for the argument.

.. option:: -no-integrated-cpp

  Perform preprocessing as a separate pass before compilation.
  By default, GCC performs preprocessing as an integrated part of
  input tokenization and parsing.
  If this option is provided, the appropriate language front end
  (:command:`cc1`, :command:`cc1plus`, or :command:`cc1obj` for C, C++,
  and Objective-C, respectively) is instead invoked twice,
  once for preprocessing only and once for actual compilation
  of the preprocessed input.
  This option may be useful in conjunction with the :option:`-B` or
  :option:`-wrapper` options to specify an alternate preprocessor or
  perform additional processing of the program source between
  normal preprocessing and compilation.

.. option:: -flarge-source-files

  Adjust GCC to expect large source files, at the expense of slower
  compilation and higher memory usage.

  Specifically, GCC normally tracks both column numbers and line numbers
  within source files and it normally prints both of these numbers in
  diagnostics.  However, once it has processed a certain number of source
  lines, it stops tracking column numbers and only tracks line numbers.
  This means that diagnostics for later lines do not include column numbers.
  It also means that options like :option:`-Wmisleading-indentation` cease to work
  at that point, although the compiler prints a note if this happens.
  Passing :option:`-flarge-source-files` significantly increases the number
  of source lines that GCC can process before it stops tracking columns.