..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _front-end:

Anatomy of a Language Front End
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A front end for a language in GCC has the following parts:

* A directory :samp:`{language}` under :samp:`gcc` containing source
  files for that front end.  See :ref:`front-end-directory`, for details.

* A mention of the language in the list of supported languages in
  :samp:`gcc/doc/install.texi`.

* A mention of the name under which the language's runtime library is
  recognized by :option:`--enable-shared=package` in the
  documentation of that option in :samp:`gcc/doc/install.texi`.

* A mention of any special prerequisites for building the front end in
  the documentation of prerequisites in :samp:`gcc/doc/install.texi`.

* Details of contributors to that front end in
  :samp:`gcc/doc/contrib.texi`.  If the details are in that front end's
  own manual then there should be a link to that manual's list in
  :samp:`contrib.texi`.

* Information about support for that language in
  :samp:`gcc/doc/frontends.texi`.

* Information about standards for that language, and the front end's
  support for them, in :samp:`gcc/doc/standards.texi`.  This may be a
  link to such information in the front end's own manual.

* Details of source file suffixes for that language and :option:`-x lang`
  options supported, in :samp:`gcc/doc/invoke.texi`.

* Entries in ``default_compilers`` in :samp:`gcc.cc` for source file
  suffixes for that language.

* Preferably testsuites, which may be under :samp:`gcc/testsuite` or
  runtime library directories.

  .. todo:: document somewhere how to write testsuite harnesses

* Probably a runtime library for the language, outside the :samp:`gcc`
  directory.

  .. todo:: document this further

* Details of the directories of any runtime libraries in
  :samp:`gcc/doc/sourcebuild.texi`.

* Check targets in :samp:`Makefile.def` for the top-level :samp:`Makefile`
  to check just the compiler or the compiler and runtime library for the
  language.

If the front end is added to the official GCC source repository, the
following are also necessary:

* At least one Bugzilla component for bugs in that front end and runtime
  libraries.  This category needs to be added to the Bugzilla database.

* Normally, one or more maintainers of that front end listed in
  :samp:`MAINTAINERS`.

* Mentions on the GCC web site in :samp:`index.html` and
  :samp:`frontends.html`, with any relevant links on
  :samp:`readings.html`.  (Front ends that are not an official part of
  GCC may also be listed on :samp:`frontends.html`, with relevant links.)

* A news item on :samp:`index.html`, and possibly an announcement on the
  gcc-announce@gcc.gnu.org mailing list.

* The front end's manuals should be mentioned in
  :samp:`maintainer-scripts/update_web_docs_git` (see :ref:`building_documentation`)
  and the online manuals should be linked to from
  :samp:`onlinedocs/index.html`.

* Any old releases or CVS repositories of the front end, before its
  inclusion in GCC, should be made available on the GCC web site at
  https://gcc.gnu.org/pub/gcc/old-releases/.

* The release and snapshot script :samp:`maintainer-scripts/gcc_release`
  should be updated to generate appropriate tarballs for this front end.

* If this front end includes its own version files that include the
  current date, :samp:`maintainer-scripts/update_version` should be
  updated accordingly.

.. toctree::
  :maxdepth: 2


.. _front-end-directory:

The Front End language Directory
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A front end :samp:`{language}` directory contains the source files
of that front end (but not of any runtime libraries, which should be
outside the :samp:`gcc` directory).  This includes documentation, and
possibly some subsidiary programs built alongside the front end.
Certain files are special and other parts of the compiler depend on
their names:

:samp:`config-lang.in`
  This file is required in all language subdirectories.  See :ref:`front-end-config`, for details of
  its contents

:samp:`Make-lang.in`
  This file is required in all language subdirectories.  See :ref:`front-end-makefile`, for details of its
  contents.

:samp:`lang.opt`
  This file registers the set of switches that the front end accepts on
  the command line, and their :option:`--help` text.  See :ref:`options`.

:samp:`lang-specs.h`
  This file provides entries for ``default_compilers`` in
  :samp:`gcc.cc` which override the default of giving an error that a
  compiler for that language is not installed.

:samp:`{language}-tree.def`
  This file, which need not exist, defines any language-specific tree
  codes.

.. _front-end-config:

The Front End config-lang.in File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each language subdirectory contains a :samp:`config-lang.in` file.
This file is a shell script that may define some variables describing
the language:

``language``
  This definition must be present, and gives the name of the language
  for some purposes such as arguments to :option:`--enable-languages`.

``lang_requires``
  If defined, this variable lists (space-separated) language front ends
  other than C that this front end requires to be enabled (with the
  names given being their ``language`` settings).  For example, the
  Obj-C++ front end depends on the C++ and ObjC front ends, so sets
  :samp:`lang_requires="objc c++"`.

``subdir_requires``
  If defined, this variable lists (space-separated) front end directories
  other than C that this front end requires to be present.  For example,
  the Objective-C++ front end uses source files from the C++ and
  Objective-C front ends, so sets :samp:`subdir_requires="cp objc"`.

``target_libs``
  If defined, this variable lists (space-separated) targets in the top
  level :samp:`Makefile` to build the runtime libraries for this
  language, such as ``target-libobjc``.

``lang_dirs``
  If defined, this variable lists (space-separated) top level
  directories (parallel to :samp:`gcc`), apart from the runtime libraries,
  that should not be configured if this front end is not built.

``build_by_default``
  If defined to :samp:`no`, this language front end is not built unless
  enabled in a :option:`--enable-languages` argument.  Otherwise, front
  ends are built by default, subject to any special logic in
  :samp:`configure.ac` (as is present to disable the Ada front end if the
  Ada compiler is not already installed).

``boot_language``
  If defined to :samp:`yes`, this front end is built in stage1 of the
  bootstrap.  This is only relevant to front ends written in their own
  languages.

``compilers``
  If defined, a space-separated list of compiler executables that will
  be run by the driver.  The names here will each end
  with :samp:`\\$(exeext)`.

``outputs``
  If defined, a space-separated list of files that should be generated
  by :samp:`configure` substituting values in them.  This mechanism can
  be used to create a file :samp:`{language}/Makefile` from
  :samp:`{language}/Makefile.in`, but this is deprecated, building
  everything from the single :samp:`gcc/Makefile` is preferred.

``gtfiles``
  If defined, a space-separated list of files that should be scanned by
  :samp:`gengtype.cc` to generate the garbage collection tables and routines for
  this language.  This excludes the files that are common to all front
  ends.  See :ref:`type-information`.

.. _front-end-makefile:

The Front End Make-lang.in File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each language subdirectory contains a :samp:`Make-lang.in` file.  It contains
targets ``lang.hook`` (where ``lang`` is the
setting of ``language`` in :samp:`config-lang.in`) for the following
values of ``hook``, and any other Makefile rules required to
build those targets (which may if necessary use other Makefiles
specified in ``outputs`` in :samp:`config-lang.in`, although this is
deprecated).  It also adds any testsuite targets that can use the
standard rule in :samp:`gcc/Makefile.in` to the variable
``lang_checks``.

``all.cross`` ``start.encap`` ``rest.encap``

  .. todo:: exactly what goes in each of these targets?

``tags``
  Build an :command:`etags` :samp:`TAGS` file in the language subdirectory
  in the source tree.

``info``
  Build info documentation for the front end, in the build directory.
  This target is only called by :samp:`make bootstrap` if a suitable
  version of :command:`sphinx` is available, so does not need to check
  for this, and should fail if an error occurs.

``pdf``
  Build PDF documentation for the front end, in the build directory.

``html``
  Build HTML documentation for the front end, in the build directory.

``man``
  Build generated man pages for the front end from reStructuredText format
  (see :ref:`building_documentation`), in the build directory.  This target
  is only called if the necessary tools are available, but should ignore
  errors so as not to stop the build if errors occur; man pages are
  optional and the tools involved may be installed in a broken way.

``install-common``
  Install everything that is part of the front end, apart from the
  compiler executables listed in ``compilers`` in
  :samp:`config-lang.in`.

``install-info``
  Install info documentation for the front end, if it is present in the
  source directory.  This target should have dependencies on info files
  that should be installed.

``install-man``
  Install man pages for the front end.  This target should ignore
  errors.

``install-plugin``
  Install headers needed for plugins.

``srcextra``
  Copies its dependencies into the source directory.  This generally should
  be used for generated files such as Bison output files which are not
  version-controlled, but should be included in any release tarballs.  This
  target will be executed during a bootstrap if
  :samp:`--enable-generated-files-in-srcdir` was specified as a
  :samp:`configure` option.

``srcinfo`` ``srcman``
  Copies its dependencies into the source directory.  These targets will be
  executed during a bootstrap if :samp:`--enable-generated-files-in-srcdir`
  was specified as a :samp:`configure` option.

``uninstall``
  Uninstall files installed by installing the compiler.  This is
  currently documented not to be supported, so the hook need not do
  anything.

``mostlyclean`` ``clean`` ``distclean`` ``maintainer-clean``
  The language parts of the standard GNU
  :samp:`*clean` targets.  For GCC, ``maintainer-clean`` should delete
  all generated files in the source directory that are not version-controlled,
  but should not delete anything that is.

  :samp:`Make-lang.in` must also define a variable ``lang_OBJS``
  to a list of host object files that are used by that language.