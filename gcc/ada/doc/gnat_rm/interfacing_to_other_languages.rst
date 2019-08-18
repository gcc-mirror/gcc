.. _Interfacing_to_Other_Languages:

******************************
Interfacing to Other Languages
******************************

The facilities in Annex B of the Ada Reference Manual are fully
implemented in GNAT, and in addition, a full interface to C++ is
provided.

.. _Interfacing_to_C:

Interfacing to C
================

Interfacing to C with GNAT can use one of two approaches:

*
  The types in the package ``Interfaces.C`` may be used.
*
  Standard Ada types may be used directly.  This may be less portable to
  other compilers, but will work on all GNAT compilers, which guarantee
  correspondence between the C and Ada types.

Pragma ``Convention C`` may be applied to Ada types, but mostly has no
effect, since this is the default.  The following table shows the
correspondence between Ada scalar types and the corresponding C types.


======================== ==================================================================
Ada Type                 C Type
======================== ==================================================================
``Integer``              ``int``
``Short_Integer``        ``short``
``Short_Short_Integer``  ``signed char``
``Long_Integer``         ``long``
``Long_Long_Integer``    ``long long``
``Short_Float``          ``float``
``Float``                ``float``
``Long_Float``           ``double``
``Long_Long_Float``      This is the longest floating-point type supported by the hardware.
======================== ==================================================================

Additionally, there are the following general correspondences between Ada
and C types:

*
  Ada enumeration types map to C enumeration types directly if pragma
  ``Convention C`` is specified, which causes them to have a length of
  32 bits, except for boolean types which map to C99 ``bool`` and for
  which the length is 8 bits.
  Without pragma ``Convention C``, Ada enumeration types map to
  8, 16, or 32 bits (i.e., C types ``signed char``, ``short``,
  ``int``, respectively) depending on the number of values passed.
  This is the only case in which pragma ``Convention C`` affects the
  representation of an Ada type.

*
  Ada access types map to C pointers, except for the case of pointers to
  unconstrained types in Ada, which have no direct C equivalent.

*
  Ada arrays map directly to C arrays.

*
  Ada records map directly to C structures.

*
  Packed Ada records map to C structures where all members are bit fields
  of the length corresponding to the ``type'Size`` value in Ada.

.. _Interfacing_to_C++:

Interfacing to C++
==================

The interface to C++ makes use of the following pragmas, which are
primarily intended to be constructed automatically using a binding generator
tool, although it is possible to construct them by hand.

Using these pragmas it is possible to achieve complete
inter-operability between Ada tagged types and C++ class definitions.
See :ref:`Implementation_Defined_Pragmas`, for more details.

:samp:`pragma CPP_Class ([Entity =>] {LOCAL_NAME})`
  The argument denotes an entity in the current declarative region that is
  declared as a tagged or untagged record type. It indicates that the type
  corresponds to an externally declared C++ class type, and is to be laid
  out the same way that C++ would lay out the type.

  Note: Pragma ``CPP_Class`` is currently obsolete. It is supported
  for backward compatibility but its functionality is available
  using pragma ``Import`` with ``Convention`` = ``CPP``.


:samp:`pragma CPP_Constructor ([Entity =>] {LOCAL_NAME})`
  This pragma identifies an imported function (imported in the usual way
  with pragma ``Import``) as corresponding to a C++ constructor.

A few restrictions are placed on the use of the ``Access`` attribute
in conjunction with subprograms subject to convention ``CPP``: the
attribute may be used neither on primitive operations of a tagged
record type with convention ``CPP``, imported or not, nor on
subprograms imported with pragma ``CPP_Constructor``.

In addition, C++ exceptions are propagated and can be handled in an
``others`` choice of an exception handler. The corresponding Ada
occurrence has no message, and the simple name of the exception identity
contains ``Foreign_Exception``. Finalization and awaiting dependent
tasks works properly when such foreign exceptions are propagated.

It is also possible to import a C++ exception using the following syntax:


::

  LOCAL_NAME : exception;
  pragma Import (Cpp,
    [Entity =>] LOCAL_NAME,
    [External_Name =>] static_string_EXPRESSION);


The ``External_Name`` is the name of the C++ RTTI symbol. You can then
cover a specific C++ exception in an exception handler.

.. _Interfacing_to_COBOL:

Interfacing to COBOL
====================

Interfacing to COBOL is achieved as described in section B.4 of
the Ada Reference Manual.

.. _Interfacing_to_Fortran:

Interfacing to Fortran
======================

Interfacing to Fortran is achieved as described in section B.5 of the
Ada Reference Manual.  The pragma ``Convention Fortran``, applied to a
multi-dimensional array causes the array to be stored in column-major
order as required for convenient interface to Fortran.

.. _Interfacing_to_non-GNAT_Ada_code:

Interfacing to non-GNAT Ada code
================================

It is possible to specify the convention ``Ada`` in a pragma
``Import`` or pragma ``Export``.  However this refers to
the calling conventions used by GNAT, which may or may not be
similar enough to those used by some other Ada 83 / Ada 95 / Ada 2005
compiler to allow interoperation.

If arguments types are kept simple, and if the foreign compiler generally
follows system calling conventions, then it may be possible to integrate
files compiled by other Ada compilers, provided that the elaboration
issues are adequately addressed (for example by eliminating the
need for any load time elaboration).

In particular, GNAT running on VMS is designed to
be highly compatible with the DEC Ada 83 compiler, so this is one
case in which it is possible to import foreign units of this type,
provided that the data items passed are restricted to simple scalar
values or simple record types without variants, or simple array
types with fixed bounds.
