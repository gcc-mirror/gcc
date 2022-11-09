..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: options to control warnings, warning messages, messages, warning, suppressing warnings

.. _warnings:

Warnings
********

Warnings are diagnostic messages that report constructions that
are not inherently erroneous but that are risky or suggest there
is likely to be a bug in the program.  Unless :option:`-Werror` is
specified, they do not prevent compilation of the program.

.. option:: -Wall

  .. index:: -Wall, -Wno-all

  Turns on all warnings messages.  Warnings are not a defined part of
  the D language, and all constructs for which this may generate a
  warning message are valid code.

.. option:: -Walloca

  .. index:: -Walloca

  This option warns on all uses of "alloca" in the source.

.. option:: -Walloca-larger-than=n

  .. index:: -Walloca-larger-than, -Wno-alloca-larger-than

  Warn on unbounded uses of alloca, and on bounded uses of alloca
  whose bound can be larger than :samp:`{n}` bytes.
  :option:`-Wno-alloca-larger-than` disables
  :option:`-Walloca-larger-than` warning and is equivalent to
  :option:`-Walloca-larger-than=SIZE_MAX` or larger.

.. option:: -Wcast-result

  .. index:: -Wcast-result, -Wno-cast-result

  Warn about casts that will produce a null or zero result.  Currently
  this is only done for casting between an imaginary and non-imaginary
  data type, or casting between a D and C++ class.

.. option:: -Wno-deprecated

  .. index:: -Wdeprecated, -Wno-deprecated

  Do not warn about usage of deprecated features and symbols with
  ``deprecated`` attributes.

.. option:: -Werror

  .. index:: -Werror, -Wno-error

  Turns all warnings into errors.

.. option:: -Wspeculative

  .. index:: -Wspeculative, -Wno-speculative

  List all error messages from speculative compiles, such as
  ``__traits(compiles, ...)``.  This option does not report
  messages as warnings, and these messages therefore never become
  errors when the :option:`-Werror` option is also used.

.. option:: -Wtemplates

  .. index:: -Wtemplates, -Wno-templates

  Warn when a template instantiation is encountered.  Some coding
  rules disallow templates, and this may be used to enforce that rule.

.. option:: -Wunknown-pragmas

  .. index:: -Wunknown-pragmas, -Wno-unknown-pragmas

  Warn when a ``pragma()`` is encountered that is not understood by
  :command:`gdc`.  This differs from :option:`-fignore-unknown-pragmas`
  where a pragma that is part of the D language, but not implemented by
  the compiler, won't get reported.

.. option:: -Wno-varargs

  .. index:: Wvarargs, Wno-varargs

  Do not warn upon questionable usage of the macros used to handle variable
  arguments like ``va_start``.

.. option:: -fignore-unknown-pragmas

  .. index:: -fignore-unknown-pragmas, -fno-ignore-unknown-pragmas

  Turns off errors for unsupported pragmas.

.. option:: -fmax-errors=n

  .. index:: -fmax-errors

  Limits the maximum number of error messages to :samp:`{n}`, at which point
  :command:`gdc` bails out rather than attempting to continue processing the
  source code.  If :samp:`{n}` is 0 (the default), there is no limit on the
  number of error messages produced.

.. option:: -fsyntax-only

  .. index:: -fsyntax-only, -fno-syntax-only

  Check the code for syntax errors, but do not actually compile it.  This
  can be used in conjunction with :option:`-fdoc` or :option:`-H` to generate
  files for each module present on the command-line, but no other output
  file.

.. option:: -ftransition=id

  .. index:: -ftransition

  Report additional information about D language changes identified by
  :samp:`{id}`.  The following values are supported:

  :samp:`all`
    List information on all D language transitions.

  :samp:`complex`
    List all usages of complex or imaginary types.

  :samp:`field`
    List all non-mutable fields which occupy an object instance.

  :samp:`in`
    List all usages of ``in`` on parameter.

  :samp:`nogc`
    List all hidden GC allocations.

  :samp:`templates`
    List statistics on template instantiations.

  :samp:`tls`
    List all variables going into thread local storage.

  :samp:`vmarkdown`
    List instances of Markdown replacements in Ddoc.
