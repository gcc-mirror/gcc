..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: preprocessor, options, preprocessor, CPP, FPP, Conditional compilation, Preprocessing, preprocessor, include file handling

.. _preprocessing-options:

Enable and customize preprocessing
**********************************

Many Fortran compilers including GNU Fortran allow passing the source code
through a C preprocessor (CPP; sometimes also called the Fortran preprocessor,
FPP) to allow for conditional compilation.  In the case of GNU Fortran,
this is the GNU C Preprocessor in the traditional mode.  On systems with
case-preserving file names, the preprocessor is automatically invoked if the
filename extension is :samp:`.F`, :samp:`.FOR`, :samp:`.FTN`, :samp:`.fpp`,
:samp:`.FPP`, :samp:`.F90`, :samp:`.F95`, :samp:`.F03` or :samp:`.F08`.  To manually
invoke the preprocessor on any file, use :option:`-cpp`, to disable
preprocessing on files where the preprocessor is run automatically, use
:option:`-nocpp`.

If a preprocessed file includes another file with the Fortran ``INCLUDE``
statement, the included file is not preprocessed.  To preprocess included
files, use the equivalent preprocessor statement ``#include``.

If GNU Fortran invokes the preprocessor, ``__GFORTRAN__``
is defined.  The macros ``__GNUC__``, ``__GNUC_MINOR__`` and
``__GNUC_PATCHLEVEL__`` can be used to determine the version of the
compiler.  See :ref:`cpp:top` for details.

GNU Fortran supports a number of ``INTEGER`` and ``REAL`` kind types
in additional to the kind types required by the Fortran standard.
The availability of any given kind type is architecture dependent.  The
following pre-defined preprocessor macros can be used to conditionally
include code for these additional kind types: ``__GFC_INT_1__``,
``__GFC_INT_2__``, ``__GFC_INT_8__``, ``__GFC_INT_16__``,
``__GFC_REAL_10__``, and ``__GFC_REAL_16__``.

While CPP is the de-facto standard for preprocessing Fortran code,
Part 3 of the Fortran 95 standard (ISO/IEC 1539-3:1998) defines
Conditional Compilation, which is not widely used and not directly
supported by the GNU Fortran compiler.  You can use the program coco
to preprocess such files (http://www.daniellnagle.com/coco.html).

The following options control preprocessing of Fortran code:

.. index:: cpp, fpp, preprocessor, enable, preprocessor, disable

.. option:: -cpp, -nocpp

  Enable preprocessing. The preprocessor is automatically invoked if
  the file extension is :samp:`.fpp`, :samp:`.FPP`,  :samp:`.F`, :samp:`.FOR`,
  :samp:`.FTN`, :samp:`.F90`, :samp:`.F95`, :samp:`.F03` or :samp:`.F08`. Use
  this option to manually enable preprocessing of any kind of Fortran file.

  To disable preprocessing of files with any of the above listed extensions,
  use the negative form: :option:`-nocpp`.

  The preprocessor is run in traditional mode. Any restrictions of the
  file-format, especially the limits on line length, apply for
  preprocessed output as well, so it might be advisable to use the
  :option:`-ffree-line-length-none` or :option:`-ffixed-line-length-none`
  options.

.. index:: dM, preprocessor, debugging, debugging, preprocessor

.. option:: -dM

  Instead of the normal output, generate a list of ``'#define'``
  directives for all the macros defined during the execution of the
  preprocessor, including predefined macros. This gives you a way
  of finding out what is predefined in your version of the preprocessor.
  Assuming you have no file :samp:`foo.f90`, the command

  .. code-block:: bash

      touch foo.f90; gfortran -cpp -E -dM foo.f90

  will show all the predefined macros.

.. index:: dD, preprocessor, debugging, debugging, preprocessor

.. option:: -dD

  Like :option:`-dM` except in two respects: it does not include the
  predefined macros, and it outputs both the ``#define`` directives
  and the result of preprocessing. Both kinds of output go to the
  standard output file.

.. index:: dN, preprocessor, debugging, debugging, preprocessor

.. option:: -dN

  Like :option:`-dD`, but emit only the macro names, not their expansions.

.. index:: dU, preprocessor, debugging, debugging, preprocessor

.. option:: -dU

  Like dD except that only macros that are expanded, or whose
  definedness is tested in preprocessor directives, are output; the
  output is delayed until the use or test of the macro; and ``'#undef'``
  directives are also output for macros tested but undefined at the time.

.. index:: dI, preprocessor, debugging, debugging, preprocessor

.. option:: -dI

  Output ``'#include'`` directives in addition to the result
  of preprocessing.

.. index:: fworking-directory, preprocessor, working directory

.. option:: -fworking-directory

  Enable generation of linemarkers in the preprocessor output that will
  let the compiler know the current working directory at the time of
  preprocessing. When this option is enabled, the preprocessor will emit,
  after the initial linemarker, a second linemarker with the current
  working directory followed by two slashes. GCC will use this directory,
  when it is present in the preprocessed input, as the directory emitted
  as the current working directory in some debugging information formats.
  This option is implicitly enabled if debugging information is enabled,
  but this can be inhibited with the negated form
  :option:`-fno-working-directory`. If the :option:`-P` flag is present
  in the command line, this option has no effect, since no ``#line``
  directives are emitted whatsoever.

.. index:: idirafter dir, preprocessing, include path

.. option:: -idirafter {dir}

  Search :samp:`{dir}` for include files, but do it after all directories
  specified with :option:`-I` and the standard system directories have
  been exhausted. :samp:`{dir}` is treated as a system include directory.
  If dir begins with ``=``, then the ``=`` will be replaced by
  the sysroot prefix; see :option:`--sysroot` and :option:`-isysroot`.

.. index:: imultilib dir, preprocessing, include path

.. option:: -imultilib {dir}

  Use :samp:`{dir}` as a subdirectory of the directory containing target-specific
  C++ headers.

.. index:: iprefix prefix, preprocessing, include path

.. option:: -iprefix {prefix}

  Specify :samp:`{prefix}` as the prefix for subsequent :option:`-iwithprefix`
  options. If the :samp:`{prefix}` represents a directory, you should include
  the final ``'/'``.

.. index:: isysroot dir, preprocessing, include path

.. option:: -isysroot {dir}

  This option is like the :option:`--sysroot` option, but applies only to
  header files. See the :option:`--sysroot` option for more information.

.. index:: iquote dir, preprocessing, include path

.. option:: -iquote {dir}

  Search :samp:`{dir}` only for header files requested with ``#include "file"`` ;
  they are not searched for ``#include <file>``, before all directories
  specified by :option:`-I` and before the standard system directories. If
  :samp:`{dir}` begins with ``=``, then the ``=`` will be replaced by the
  sysroot prefix; see :option:`--sysroot` and :option:`-isysroot`.

.. index:: isystem dir, preprocessing, include path

.. option:: -isystem {dir}

  Search :samp:`{dir}` for header files, after all directories specified by
  :option:`-I` but before the standard system directories. Mark it as a
  system directory, so that it gets the same special treatment as is
  applied to the standard system directories. If :samp:`{dir}` begins with
  ``=``, then the ``=`` will be replaced by the sysroot prefix;
  see :option:`--sysroot` and :option:`-isysroot`.

.. index:: nostdinc

.. option:: -nostdinc

  Do not search the standard system directories for header files. Only
  the directories you have specified with :option:`-I` options (and the
  directory of the current file, if appropriate) are searched.

.. index:: undef

.. option:: -undef

  Do not predefine any system-specific or GCC-specific macros.
  The standard predefined macros remain defined.

.. index:: Apredicate=answer, preprocessing, assertion

.. option:: -Apredicate={answer}

  Make an assertion with the predicate :samp:`{predicate}` and answer :samp:`{answer}`.
  This form is preferred to the older form -A predicate(answer), which is still
  supported, because it does not use shell special characters.

.. index:: A-predicate=answer, preprocessing, assertion

.. option:: -A-predicate={answer}

  Cancel an assertion with the predicate :samp:`{predicate}` and answer :samp:`{answer}`.

.. index:: C, preprocessing, keep comments

.. option:: -C

  Do not discard comments. All comments are passed through to the output
  file, except for comments in processed directives, which are deleted
  along with the directive.

  You should be prepared for side effects when using :option:`-C` ; it causes
  the preprocessor to treat comments as tokens in their own right. For example,
  comments appearing at the start of what would be a directive line have the
  effect of turning that line into an ordinary source line, since the first
  token on the line is no longer a ``'#'``.

  Warning: this currently handles C-Style comments only. The preprocessor
  does not yet recognize Fortran-style comments.

.. index:: CC, preprocessing, keep comments

.. option:: -CC

  Do not discard comments, including during macro expansion. This is like
  :option:`-C`, except that comments contained within macros are also passed
  through to the output file where the macro is expanded.

  In addition to the side-effects of the :option:`-C` option, the :option:`-CC`
  option causes all C++-style comments inside a macro to be converted to C-style
  comments. This is to prevent later use of that macro from inadvertently
  commenting out the remainder of the source line. The :option:`-CC` option
  is generally used to support lint comments.

  Warning: this currently handles C- and C++-Style comments only. The
  preprocessor does not yet recognize Fortran-style comments.

.. index:: Dname, preprocessing, define macros

.. option:: -Dname

  Predefine name as a macro, with definition ``1``.

.. index:: Dname=definition, preprocessing, define macros

.. option:: -Dname={definition}

  The contents of :samp:`{definition}` are tokenized and processed as if they
  appeared during translation phase three in a ``'#define'`` directive.
  In particular, the definition will be truncated by embedded newline
  characters.

  If you are invoking the preprocessor from a shell or shell-like program
  you may need to use the shell's quoting syntax to protect characters such
  as spaces that have a meaning in the shell syntax.

  If you wish to define a function-like macro on the command line, write
  its argument list with surrounding parentheses before the equals sign
  (if any). Parentheses are meaningful to most shells, so you will need
  to quote the option. With sh and csh, ``-D'name(args...)=definition'``
  works.

  :option:`-D` and :option:`-U` options are processed in the order they are
  given on the command line. All -imacros file and -include file options
  are processed after all -D and -U options.

.. index:: H

.. option:: -H

  Print the name of each header file used, in addition to other normal
  activities. Each name is indented to show how deep in the ``'#include'``
  stack it is.

.. index:: P, preprocessing, no linemarkers

.. option:: -P

  Inhibit generation of linemarkers in the output from the preprocessor.
  This might be useful when running the preprocessor on something that
  is not C code, and will be sent to a program which might be confused
  by the linemarkers.

.. index:: Uname, preprocessing, undefine macros

.. option:: -Uname

  Cancel any previous definition of :samp:`{name}`, either built in or provided
  with a :option:`-D` option.