..
  Copyright 1988-2022 Free Software Foundation, Inc.
  This is part of the GCC manual.
  For copying conditions, see the copyright.rst file.

.. index:: Standards

.. _standards:

Standards
*********

Fortran is developed by the Working Group 5 of Sub-Committee 22 of the
Joint Technical Committee 1 of the International Organization for
Standardization and the International Electrotechnical Commission (IEC).
This group is known as `WG5 <http://www.nag.co.uk/sc22wg5/>`_.
Official Fortran standard documents are available for purchase
from ISO; a collection of free documents (typically final drafts) are
also available on the `wiki <https://gcc.gnu.org/wiki/GFortranStandards>`_.

The GNU Fortran compiler implements ISO/IEC 1539:1997 (Fortran 95).
As such, it can also compile essentially all standard-compliant
Fortran 90 and Fortran 77 programs.  It also supports the ISO/IEC
TR-15581 enhancements to allocatable arrays.

GNU Fortran also supports almost all of ISO/IEC 1539-1:2004
(Fortran 2003) and ISO/IEC 1539-1:2010 (Fortran 2008).
It has partial support for features introduced in ISO/IEC
1539:2018 (Fortran 2018), the most recent version of the Fortran
language standard, including full support for the Technical Specification
``Further Interoperability of Fortran with C`` (ISO/IEC TS 29113:2012).
More details on support for these standards can be
found in the following sections of the documentation.

Additionally, the GNU Fortran compilers supports the OpenMP specification
(version 4.5 and partial support of the features of the 5.0 version,
https://openmp.org/specifications/).
There also is support for the OpenACC specification (targeting
version 2.6, https://www.openacc.org/).  See
https://gcc.gnu.org/wiki/OpenACC for more information.

.. index:: Varying length strings, strings, varying length, conditional compilation

.. _fortran-95-status:

Fortran 95 status
^^^^^^^^^^^^^^^^^

The Fortran 95 standard specifies in Part 2 (ISO/IEC 1539-2:2000)
varying length character strings.  While GNU Fortran currently does not
support such strings directly, there exist two Fortran implementations
for them, which work with GNU Fortran. One can be found at
http://user.astro.wisc.edu/~townsend/static.php?ref=iso-varying-string.

Deferred-length character strings of Fortran 2003 supports part of
the features of ``ISO_VARYING_STRING`` and should be considered as
replacement. (Namely, allocatable or pointers of the type
``character(len=:)``.)

Part 3 of the Fortran 95 standard (ISO/IEC 1539-3:1998) defines
Conditional Compilation, which is not widely used and not directly
supported by the GNU Fortran compiler.  You can use the program coco
to preprocess such files (http://www.daniellnagle.com/coco.html).

.. _fortran-2003-status:

Fortran 2003 status
^^^^^^^^^^^^^^^^^^^

GNU Fortran implements the Fortran 2003 (ISO/IEC 1539-1:2004) standard
except for finalization support, which is incomplete.
See the
`Fortran 2003 wiki page <https://gcc.gnu.org/wiki/Fortran2003>`_ for a full list
of new features introduced by Fortran 2003 and their implementation status.

.. _fortran-2008-status:

Fortran 2008 status
^^^^^^^^^^^^^^^^^^^

The GNU Fortran compiler supports almost all features of Fortran 2008;
the `Fortran 2008 wiki <https://gcc.gnu.org/wiki/Fortran2008Status>`_
has some information about the current implementation status.
In particular, the following are not yet supported:

* ``DO CONCURRENT`` and ``FORALL`` do not recognize a
  type-spec in the loop header.

* The change to permit any constant expression in subscripts and
  nested implied-do limits in a ``DATA`` statement has not been implemented.

.. _fortran-2018-status:

Fortran 2018 status
^^^^^^^^^^^^^^^^^^^

Fortran 2018 (ISO/IEC 1539:2018) is the most recent version
of the Fortran language standard.  GNU Fortran implements some of the
new features of this standard:

* All Fortran 2018 features derived from ISO/IEC TS 29113:2012,
  'Further Interoperability of Fortran with C', are supported by GNU Fortran.
  This includes assumed-type and assumed-rank objects and
  the ``SELECT RANK`` construct as well as the parts relating to
  ``BIND(C)`` functions.
  See also :ref:`further-interoperability-of-fortran-with-c`.

* GNU Fortran supports a subset of features derived from ISO/IEC TS 18508:2015,
  'Additional Parallel Features in Fortran':

  * The new atomic ADD, CAS, FETCH and ADD/OR/XOR, OR and XOR intrinsics.

  * The ``CO_MIN`` and ``CO_MAX`` and ``SUM`` reduction intrinsics,
    and the ``CO_BROADCAST`` and ``CO_REDUCE`` intrinsic, except that those
    do not support polymorphic types or types with allocatable, pointer or
    polymorphic components.

  * Events (``EVENT POST``, ``EVENT WAIT``, ``EVENT_QUERY``).

  * Failed images (``FAIL IMAGE``, ``IMAGE_STATUS``,
    ``FAILED_IMAGES``, ``STOPPED_IMAGES``).

* An ``ERROR STOP`` statement is permitted in a ``PURE``
  procedure.

* GNU Fortran supports the ``IMPLICIT NONE`` statement with an
  ``implicit-none-spec-list``.

* The behavior of the ``INQUIRE`` statement with the ``RECL=``
  specifier now conforms to Fortran 2018.