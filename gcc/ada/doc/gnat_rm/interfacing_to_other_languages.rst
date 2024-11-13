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
cover a specific C++ exception in an exception handler. If the string
ends with "'Class", as if referencing the Class attribute of the C++
type, that enables "class-wide" type matching, i.e., instances of C++
classes derived from the one denoted by the RTTI symbol, that would be
caught by C++ handlers for that type, will also be caught by Ada
handlers for ``Entity``. For non-class-wide RTTI symbols imported from
C++, only exact type matches will be handled. C++ rethrown (dependent)
exceptions are not distinguishable from the corresponding primary
exceptions: they are handled exactly as if the primary exception had
been raised.

With imported exceptions, especially with base-type matching, a single
handled_sequence_of_statements may have exception handlers with
choices that cover the same C++ types in ways that GNAT cannot detect.
For example, C++ classes ``base`` and ``derived`` may be imported as
exceptions with base-type matching, but GNAT does not know that they
are related by inheritance, only the runtime will know it. Given:

::

   exception
     when Derived_Exception => null;
     when Base_Exception => null;
     when others => null;

the earliest handler that matches the type of the raised object will
be selected. If an instance of ``derived`` or a further derived type
is raised, the first handler will be used; if an instance of ``base``
that is not an instance of ``derived`` is raised, the second handler
will be used; raised objects that are not instances of ``base`` will
be handled by the ``others`` handler. However, if the handlers were
reordered (``others`` must remain last), the ``Derived_Exception``
handler would never be used, because ``Base_Exception`` would match
any instances of ``derived`` before ``Derived_Exception`` or
``others`` handlers were considered. Mixing exact-type and base-type
matching exceptions may also involve overlapping handlers that GNAT
will not reject: an exact-type ``Base_Only_Exception`` handler placed
before ``Base_Exception`` will handle instances of ``base``, whereas
instances of derived types will be handled by
``Base_Exception``. Swapping them will cause ``Base_Exception`` to
handle all instances of ``base`` and derived types, so that a
subsequent handler for ``Base_Only_Exception`` will never be selected.

The C++ object associated with a C++ ``Exception_Occurrence`` may be
obtained by calling the ``GNAT.CPP_Exceptions.Get_Object_Address``
function. There are convenience generic wrappers named ``Get_Object``,
``Get_Access_To_Object``, and ``Get_Access_To_Tagged_Object``,
parameterized on the expected Ada type. Note that, for exceptions
imported from C++, the address of the object is that of the subobject
of the type associated with the exception, which may have a different
address from that of the full object; for C++ exceptions handled by
``others`` handlers, however, the address of the full object is
returned.

E.g., if the imported exception uses the RTTI symbol for the base
class, followed by "'Class", and the C++ code raises (throws) an
instance of a derived class, a handler for that imported exception
will catch this ``Exception_Occurrence``, and ``Get_Object_Address``
will return the address of the base subobject of the raised derived
object; ``Get_Object``, ``Get_Access_To_Object`` and
``Get_Access_To_Tagged_Object`` only convert that address to the
parameterized type, so the specified type ought to be a type that
imports the C++ type whose RTTI symbol was named in the declared
exception, i.e., base, not derived or any other type. GNAT cannot
detect or report if a type is named that does not match the handler's
RTTI-specified type.

For ``others`` handlers, and for exact type matches, the full object
is obtained. The ``Get_Type_Info`` function that takes an
``Exception_Occurrence`` argument can be used to verify the type of
the C++ object raised as an exception. The other ``Get_Type_Info``
function, that takes an ``Exception_Id``, obtains the type expected by
the handler, and no such type exists for ``others`` handlers.
``GNAT.CPP.Std.Name`` can then convert the opaque
``GNAT.CPP.Std.Type_Info_Ptr`` access to ``std::type_info`` objects,
returned by either ``Get_Type_Info`` function, to a C++ mangled type
name.

If an ``Exception_Occurrence`` was raised from C++, or following C++
conventions, ``GNAT.Exception_Actions.Exception_Language`` will return
``EL_Cpp``, whether the exception handler is an imported C++ exception
or ``others``. ``GNAT.Exception_Actions.Is_Foreign_Exception`` returns
True for all of these, as well as for any case in which
``Exception_Language`` is not ``EL_Ada``.

::

    --  Given the following partial package specification:

      Base_Exception : exception;
      pragma Import (Cpp, Base_Exception, "_ZTI4base'Class");
      --  Handle instances of base, and of subclasses.

      type Base is limited tagged record
	[...]
      end record;
      pragma Import (Cpp, Base);

      type Derived is limited tagged record
	[...]
      end record;
      pragma Import (Cpp, Derived);

      type Unrelated is access procedure (B : Boolean);

      function Get_Base_Obj_Acc is
	new Get_Access_To_Tagged_Object (Base);
      function Get_Derived_Obj_Acc is
	new Get_Access_To_Tagged_Object (Derived);
      function Get_Unrelated_Obj_Acc is
	new Get_Access_To_Object (Unrelated);

      procedure Raise_Derived;
      --  Raises an instance of derived (with a base subobject).


    --  The comments next to each statement indicate the behavior of
    --  the following pseudocode blocks:

    begin
      Raise_Derived;
    exception
      when BEx : Base_Exception =>
	?? := Is_Foreign_Exception (BEx);  --  True
	?? := Exception_Language (BEx);    --  EL_Cpp
	?? := Name (Get_Type_Info (BEx));  --  "7derived"
	?? := Name (Get_Type_Info (Exception_Identity (BEx)));  --  "4base"
	?? := Get_Object_Address (BEx);    --  base subobject in derived object
	?? := Get_Base_Obj_Acc (BEx):      --  ditto, as access to Base
	?? := Get_Derived_Obj_Acc (BEx):   --  ditto, NO ERROR DETECTED!
	?? := Get_Unrelated_Obj_Acc (BEx): --  ditto, NO ERROR DETECTED!
    end;


    begin
      Raise_Derived;
    exception
      when BEx : others =>
	?? := Is_Foreign_Exception (BEx);  --  True
	?? := Exception_Language (BEx);    --  EL_Cpp
	?? := Name (Get_Type_Info (BEx));  --  "7derived"
	?? := Get_Type_Info (Exception_Identity (BEx));  --  null
	?? := Get_Object_Address (BEx);    --  full derived object
	?? := Get_Derived_Obj_Acc (BEx):   --  ditto, as access to Derived
	?? := Get_Base_Obj_Acc (BEx):      --  ditto, NO ERROR DETECTED!
	?? := Get_Unrelated_Obj_Acc (BEx): --  ditto, NO ERROR DETECTED!
    end;

The calls marked with ``NO ERROR DETECTED!`` will compile sucessfully,
even though the types specified in the specializations of the generic
function do not match the type of the exception object that the
function is expected to return. Mismatches between derived and base
types are particularly relevant because they will appear to work as
long as there isn't any offset between pointers to these types. This
may hold in many cases, but is subject to change with various possible
changes to the derived class.

The ``GNAT.CPP.Std`` package offers interfaces corresponding to the
C++ standard type ``std::type_info``. Function ``To_Type_Info_Ptr``
builds an opaque ``Type_Info_Ptr`` to reference a ``std::type_info``
object at a given ``System.Address``.


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
