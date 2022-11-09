..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. option:: -I {dir}, -iquote {dir}, -isystem {dir}, -idirafter {dir}

  Add the directory :samp:`{dir}` to the list of directories to be searched
  for header files during preprocessing.

  .. only:: cpp

    See :ref:`search-path`.

  If :samp:`{dir}` begins with :samp:`=` or ``$SYSROOT``, then the :samp:`=`
  or ``$SYSROOT`` is replaced by the sysroot prefix; see
  :option:`--sysroot` and :option:`-isysroot`.

  Directories specified with :option:`-iquote` apply only to the quote
  form of the directive, ``#include "file"``.
  Directories specified with :option:`-I`, :option:`-isystem`,
  or :option:`-idirafter` apply to lookup for both the
  ``#include "file"`` and
  ``#include <file>`` directives.

  You can specify any number or combination of these options on the
  command line to search for header files in several directories.
  The lookup order is as follows:

  * For the quote form of the include directive, the directory of the current
    file is searched first.

  * For the quote form of the include directive, the directories specified
    by :option:`-iquote` options are searched in left-to-right order,
    as they appear on the command line.

  * Directories specified with :option:`-I` options are scanned in
    left-to-right order.

  * Directories specified with :option:`-isystem` options are scanned in
    left-to-right order.

  * Standard system directories are scanned.

  * Directories specified with :option:`-idirafter` options are scanned in
    left-to-right order.

  You can use :option:`-I` to override a system header
  file, substituting your own version, since these directories are
  searched before the standard system header file directories.
  However, you should
  not use this option to add directories that contain vendor-supplied
  system header files; use :option:`-isystem` for that.

  The :option:`-isystem` and :option:`-idirafter` options also mark the directory
  as a system directory, so that it gets the same special treatment that
  is applied to the standard system directories.

  .. only:: cpp

    See :ref:`system-headers`.


  If a standard system include directory, or a directory specified with
  :option:`-isystem`, is also specified with :option:`-I`, the :option:`-I`
  option is ignored.  The directory is still searched but as a
  system directory at its normal position in the system include chain.
  This is to ensure that GCC's procedure to fix buggy system headers and
  the ordering for the ``#include_next`` directive are not inadvertently
  changed.
  If you really need to change the search order for system directories,
  use the :option:`-nostdinc` and/or :option:`-isystem` options.

  .. only:: cpp

    See :ref:`system-headers`.


.. option:: -I-

  Split the include path.
  This option has been deprecated.  Please use :option:`-iquote` instead for
  :option:`-I` directories before the :option:`-I-` and remove the :option:`-I-`
  option.

  Any directories specified with :option:`-I`
  options before :option:`-I-` are searched only for headers requested with
  ``#include "file"`` ; they are not searched for
  ``#include <file>``.  If additional directories are
  specified with :option:`-I` options after the :option:`-I-`, those
  directories are searched for all :samp:`#include` directives.

  In addition, :option:`-I-` inhibits the use of the directory of the current
  file directory as the first search directory for ``#include
  "file"``.  There is no way to override this effect of :option:`-I-`.

  .. only:: cpp

    See :ref:`search-path`.


.. option:: -iprefix {prefix}

  Specify :samp:`{prefix}` as the prefix for subsequent :option:`-iwithprefix`
  options.  If the prefix represents a directory, you should include the
  final :samp:`/`.

.. option:: -iwithprefix {dir}, -iwithprefixbefore {dir}

  Append :samp:`{dir}` to the prefix specified previously with
  :option:`-iprefix`, and add the resulting directory to the include search
  path.  :option:`-iwithprefixbefore` puts it in the same place :option:`-I`
  would; :option:`-iwithprefix` puts it where :option:`-idirafter` would.

.. option:: -isysroot {dir}

  This option is like the :option:`--sysroot` option, but applies only to
  header files (except for Darwin targets, where it applies to both header
  files and libraries).  See the :option:`--sysroot` option for more
  information.

.. option:: -imultilib {dir}

  Use :samp:`{dir}` as a subdirectory of the directory containing
  target-specific C++ headers.

.. option:: -nostdinc

  Do not search the standard system directories for header files.
  Only the directories explicitly specified with :option:`-I`,
  :option:`-iquote`, :option:`-isystem`, and/or :option:`-idirafter`
  options (and the directory of the current file, if appropriate)
  are searched.

.. option:: -nostdinc++

  Do not search for header files in the C++-specific standard directories,
  but do still search the other standard directories.  (This option is
  used when building the C++ library.)

.. option:: -Wcomment, -Wcomments

  Warn whenever a comment-start sequence :samp:`/*` appears in a :samp:`/*`
  comment, or whenever a backslash-newline appears in a :samp:`//` comment.
  This warning is enabled by :option:`-Wall`.

.. option:: -Wtrigraphs

.. _wtrigraphs:

  Warn if any trigraphs are encountered that might change the meaning of
  the program.  Trigraphs within comments are not warned about,
  except those that would form escaped newlines.

  This option is implied by :option:`-Wall`.  If :option:`-Wall` is not
  given, this option is still enabled unless trigraphs are enabled.  To
  get trigraph conversion without warnings, but get the other
  :option:`-Wall` warnings, use :samp:`-trigraphs -Wall -Wno-trigraphs`.

.. option:: -Wundef

  Warn if an undefined identifier is evaluated in an ``#if`` directive.
  Such identifiers are replaced with zero.

.. option:: -Wno-undef

  Default setting; overrides :option:`-Wundef`.

.. option:: -Wexpansion-to-defined

  Warn whenever :samp:`defined` is encountered in the expansion of a macro
  (including the case where the macro is expanded by an :samp:`#if` directive).
  Such usage is not portable.
  This warning is also enabled by :option:`-Wpedantic` and :option:`-Wextra`.

.. option:: -Wunused-macros

  Warn about macros defined in the main file that are unused.  A macro
  is :dfn:`used` if it is expanded or tested for existence at least once.
  The preprocessor also warns if the macro has not been used at the
  time it is redefined or undefined.

  Built-in macros, macros defined on the command line, and macros
  defined in include files are not warned about.

  .. note::

    If a macro is actually used, but only used in skipped
    conditional blocks, then the preprocessor reports it as unused.  To avoid the
    warning in such a case, you might improve the scope of the macro's
    definition by, for example, moving it into the first skipped block.
    Alternatively, you could provide a dummy use with something like:

  .. code-block:: c++

    #if defined the_macro_causing_the_warning
    #endif

.. option:: -Wno-endif-labels

  Do not warn whenever an ``#else`` or an ``#endif`` are followed by text.
  This sometimes happens in older programs with code of the form

  .. code-block:: c++

    #if FOO
    ...
    #else FOO
    ...
    #endif FOO

  The second and third ``FOO`` should be in comments.
  This warning is on by default.

.. option:: -Wendif-labels

  Default setting; overrides :option:`-Wno-endif-labels`.
