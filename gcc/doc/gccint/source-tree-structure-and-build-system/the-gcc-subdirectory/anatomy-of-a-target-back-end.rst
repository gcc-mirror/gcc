..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. _back-end:

Anatomy of a Target Back End
^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A back end for a target architecture in GCC has the following parts:

* A directory :samp:`{machine}` under :samp:`gcc/config`, containing a
  machine description :samp:`{machine}.md` file (see :ref:`machine-desc`), header files :samp:`{machine}.h` and
  :samp:`{machine}-protos.h` and a source file :samp:`{machine}.c`
  (see :ref:`target-macros`),
  possibly a target Makefile fragment :samp:`t-{machine}`
  (see :ref:`target-fragment`), and maybe
  some other files.  The names of these files may be changed from the
  defaults given by explicit specifications in :samp:`config.gcc`.

* If necessary, a file :samp:`{machine}-modes.def` in the
  :samp:`{machine}` directory, containing additional machine modes to
  represent condition codes.  See :ref:`condition-code`, for further details.

* An optional :samp:`{machine}.opt` file in the :samp:`{machine}`
  directory, containing a list of target-specific options.  You can also
  add other option files using the ``extra_options`` variable in
  :samp:`config.gcc`.  See :ref:`options`.

* Entries in :samp:`config.gcc` (see :ref:`system-config`) for the systems with this target
  architecture.

* Documentation in :samp:`gcc/doc/invoke.texi` for any command-line
  options supported by this target (see :ref:`run-time-target`).  This means both entries in the summary table
  of options and details of the individual options.

* Documentation in :samp:`gcc/doc/extend.texi` for any target-specific
  attributes supported (see :ref:`target-attributes`), including where the
  same attribute is already supported on some targets, which are
  enumerated in the manual.

* Documentation in :samp:`gcc/doc/extend.texi` for any target-specific
  pragmas supported.

* Documentation in :samp:`gcc/doc/extend.texi` of any target-specific
  built-in functions supported.

* Documentation in :samp:`gcc/doc/extend.texi` of any target-specific
  format checking styles supported.

* Documentation in :samp:`gcc/doc/md.texi` of any target-specific
  constraint letters (see :ref:`machine-constraints`).

* A note in :samp:`gcc/doc/contrib.texi` under the person or people who
  contributed the target support.

* Entries in :samp:`gcc/doc/install.texi` for all target triplets
  supported with this target architecture, giving details of any special
  notes about installation for this target, or saying that there are no
  special notes if there are none.

* Possibly other support outside the :samp:`gcc` directory for runtime
  libraries.  The ``libstdc++`` porting
  manual needs to be installed as info for this to work, or to be a
  chapter of this manual.

  .. todo:: reference docs for this

The :samp:`{machine}.h` header is included very early in GCC's
standard sequence of header files, while :samp:`{machine}-protos.h`
is included late in the sequence.  Thus :samp:`{machine}-protos.h`
can include declarations referencing types that are not defined when
:samp:`{machine}.h` is included, specifically including those from
:samp:`rtl.h` and :samp:`tree.h`.  Since both RTL and tree types may not
be available in every context where :samp:`{machine}-protos.h` is
included, in this file you should guard declarations using these types
inside appropriate ``#ifdef RTX_CODE`` or ``#ifdef TREE_CODE``
conditional code segments.

If the backend uses shared data structures that require ``GTY`` markers
for garbage collection (see :ref:`type-information`), you must declare those
in :samp:`{machine}.h` rather than :samp:`{machine}-protos.h`.
Any definitions required for building libgcc must also go in
:samp:`{machine}.h`.

GCC uses the macro ``IN_TARGET_CODE`` to distinguish between
machine-specific :samp:`.c` and :samp:`.cc` files and
machine-independent :samp:`.c` and :samp:`.cc` files.  Machine-specific
files should use the directive:

.. code-block:: c++

  #define IN_TARGET_CODE 1

before including ``config.h``.

If the back end is added to the official GCC source repository, the
following are also necessary:

* An entry for the target architecture in :samp:`readings.html` on the
  GCC web site, with any relevant links.

* Details of the properties of the back end and target architecture in
  :samp:`backends.html` on the GCC web site.

* A news item about the contribution of support for that target
  architecture, in :samp:`index.html` on the GCC web site.

* Normally, one or more maintainers of that target listed in
  :samp:`MAINTAINERS`.  Some existing architectures may be unmaintained,
  but it would be unusual to add support for a target that does not have
  a maintainer when support is added.

* Target triplets covering all :samp:`config.gcc` stanzas for the target,
  in the list in :samp:`contrib/config-list.mk`.