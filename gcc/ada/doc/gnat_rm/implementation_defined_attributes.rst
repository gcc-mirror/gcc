.. _Implementation_Defined_Attributes:

*********************************
Implementation Defined Attributes
*********************************

Ada defines (throughout the Ada reference manual,
summarized in Annex K),
a set of attributes that provide useful additional functionality in all
areas of the language.  These language defined attributes are implemented
in GNAT and work as described in the Ada Reference Manual.

In addition, Ada allows implementations to define additional
attributes whose meaning is defined by the implementation.  GNAT provides
a number of these implementation-dependent attributes which can be used
to extend and enhance the functionality of the compiler.  This section of
the GNAT reference manual describes these additional attributes.  It also
describes additional implementation-dependent features of standard
language-defined attributes.

Note that any program using these attributes may not be portable to
other compilers (although GNAT implements this set of attributes on all
platforms).  Therefore if portability to other compilers is an important
consideration, you should minimize the use of these attributes.

Attribute Abort_Signal
======================
.. index:: Abort_Signal

``Standard'Abort_Signal`` (``Standard`` is the only allowed
prefix) provides the entity for the special exception used to signal
task abort or asynchronous transfer of control.  Normally this attribute
should only be used in the tasking runtime (it is highly peculiar, and
completely outside the normal semantics of Ada, for a user program to
intercept the abort exception).

Attribute Address_Size
======================
.. index:: Size of ``Address``

.. index:: Address_Size

``Standard'Address_Size`` (``Standard`` is the only allowed
prefix) is a static constant giving the number of bits in an
``Address``. It is the same value as System.Address'Size,
but has the advantage of being static, while a direct
reference to System.Address'Size is nonstatic because Address
is a private type.

Attribute Asm_Input
===================
.. index:: Asm_Input

The ``Asm_Input`` attribute denotes a function that takes two
parameters.  The first is a string, the second is an expression of the
type designated by the prefix.  The first (string) argument is required
to be a static expression, and is the constraint for the parameter,
(e.g., what kind of register is required).  The second argument is the
value to be used as the input argument.  The possible values for the
constant are the same as those used in the RTL, and are dependent on
the configuration file used to built the GCC back end.
:ref:`Machine_Code_Insertions`

Attribute Asm_Output
====================
.. index:: Asm_Output

The ``Asm_Output`` attribute denotes a function that takes two
parameters.  The first is a string, the second is the name of a variable
of the type designated by the attribute prefix.  The first (string)
argument is required to be a static expression and designates the
constraint for the parameter (e.g., what kind of register is
required).  The second argument is the variable to be updated with the
result.  The possible values for constraint are the same as those used in
the RTL, and are dependent on the configuration file used to build the
GCC back end.  If there are no output operands, then this argument may
either be omitted, or explicitly given as ``No_Output_Operands``.
:ref:`Machine_Code_Insertions`

Attribute Atomic_Always_Lock_Free
=================================
.. index:: Atomic_Always_Lock_Free

The prefix of the ``Atomic_Always_Lock_Free`` attribute is a type.
The result is a Boolean value which is True if the type has discriminants,
and False otherwise.  The result indicate whether atomic operations are
supported by the target for the given type.

Attribute Bit
=============
.. index:: Bit

``obj'Bit``, where ``obj`` is any object, yields the bit
offset within the storage unit (byte) that contains the first bit of
storage allocated for the object.  The value of this attribute is of the
type *universal_integer* and is always a nonnegative number smaller
than ``System.Storage_Unit``.

For an object that is a variable or a constant allocated in a register,
the value is zero.  (The use of this attribute does not force the
allocation of a variable to memory).

For an object that is a formal parameter, this attribute applies
to either the matching actual parameter or to a copy of the
matching actual parameter.

For an access object the value is zero.  Note that
``obj.all'Bit`` is subject to an ``Access_Check`` for the
designated object.  Similarly for a record component
``X.C'Bit`` is subject to a discriminant check and
``X(I).Bit`` and ``X(I1..I2)'Bit``
are subject to index checks.

This attribute is designed to be compatible with the DEC Ada 83 definition
and implementation of the ``Bit`` attribute.

Attribute Bit_Position
======================
.. index:: Bit_Position

``R.C'Bit_Position``, where ``R`` is a record object and ``C`` is one
of the fields of the record type, yields the bit
offset within the record contains the first bit of
storage allocated for the object.  The value of this attribute is of the
type *universal_integer*.  The value depends only on the field
``C`` and is independent of the alignment of
the containing record ``R``.

Attribute Code_Address
======================
.. index:: Code_Address
.. index:: Subprogram address

.. index:: Address of subprogram code

The ``'Address``
attribute may be applied to subprograms in Ada 95 and Ada 2005, but the
intended effect seems to be to provide
an address value which can be used to call the subprogram by means of
an address clause as in the following example:

.. code-block:: ada

  procedure K is ...

  procedure L;
  for L'Address use K'Address;
  pragma Import (Ada, L);


A call to ``L`` is then expected to result in a call to ``K``.
In Ada 83, where there were no access-to-subprogram values, this was
a common work-around for getting the effect of an indirect call.
GNAT implements the above use of ``Address`` and the technique
illustrated by the example code works correctly.

However, for some purposes, it is useful to have the address of the start
of the generated code for the subprogram.  On some architectures, this is
not necessarily the same as the ``Address`` value described above.
For example, the ``Address`` value may reference a subprogram
descriptor rather than the subprogram itself.

The ``'Code_Address`` attribute, which can only be applied to
subprogram entities, always returns the address of the start of the
generated code of the specified subprogram, which may or may not be
the same value as is returned by the corresponding ``'Address``
attribute.

Attribute Compiler_Version
==========================
.. index:: Compiler_Version

``Standard'Compiler_Version`` (``Standard`` is the only allowed
prefix) yields a static string identifying the version of the compiler
being used to compile the unit containing the attribute reference.

Attribute Constrained
=====================
.. index:: Constrained

In addition to the usage of this attribute in the Ada RM, GNAT
also permits the use of the ``'Constrained`` attribute
in a generic template
for any type, including types without discriminants. The value of this
attribute in the generic instance when applied to a scalar type or a
record type without discriminants is always ``True``. This usage is
compatible with older Ada compilers, including notably DEC Ada.


Attribute Default_Bit_Order
===========================
.. index:: Big endian

.. index:: Little endian

.. index:: Default_Bit_Order

``Standard'Default_Bit_Order`` (``Standard`` is the only
allowed prefix), provides the value ``System.Default_Bit_Order``
as a ``Pos`` value (0 for ``High_Order_First``, 1 for
``Low_Order_First``).  This is used to construct the definition of
``Default_Bit_Order`` in package ``System``.

Attribute Default_Scalar_Storage_Order
======================================
.. index:: Big endian

.. index:: Little endian

.. index:: Default_Scalar_Storage_Order

``Standard'Default_Scalar_Storage_Order`` (``Standard`` is the only
allowed prefix), provides the current value of the default scalar storage
order (as specified using pragma ``Default_Scalar_Storage_Order``, or
equal to ``Default_Bit_Order`` if unspecified) as a
``System.Bit_Order`` value. This is a static attribute.

Attribute Deref
===============
.. index:: Deref

The attribute ``typ'Deref(expr)`` where ``expr`` is of type ``System.Address`` yields
the variable of type ``typ`` that is located at the given address. It is similar
to ``(totyp (expr).all)``, where ``totyp`` is an unchecked conversion from address to
a named access-to-`typ` type, except that it yields a variable, so it can be
used on the left side of an assignment.

Attribute Descriptor_Size
=========================
.. index:: Descriptor

.. index:: Dope vector

.. index:: Descriptor_Size

Nonstatic attribute ``Descriptor_Size`` returns the size in bits of the
descriptor allocated for a type.  The result is non-zero only for unconstrained
array types and the returned value is of type universal integer.  In GNAT, an
array descriptor contains bounds information and is located immediately before
the first element of the array.

.. code-block:: ada

  type Unconstr_Array is array (Short_Short_Integer range <>) of Positive;
  Put_Line ("Descriptor size = " & Unconstr_Array'Descriptor_Size'Img);


The attribute takes into account any padding due to the alignment of the
component type. In the example above, the descriptor contains two values
of type ``Short_Short_Integer`` representing the low and high bound. But,
since ``Positive`` has an alignment of 4, the size of the descriptor is
``2 * Short_Short_Integer'Size`` rounded up to the next multiple of 32,
which yields a size of 32 bits, i.e. including 16 bits of padding.

Attribute Elaborated
====================
.. index:: Elaborated

The prefix of the ``'Elaborated`` attribute must be a unit name.  The
value is a Boolean which indicates whether or not the given unit has been
elaborated.  This attribute is primarily intended for internal use by the
generated code for dynamic elaboration checking, but it can also be used
in user programs.  The value will always be True once elaboration of all
units has been completed.  An exception is for units which need no
elaboration, the value is always False for such units.

Attribute Elab_Body
===================
.. index:: Elab_Body

This attribute can only be applied to a program unit name.  It returns
the entity for the corresponding elaboration procedure for elaborating
the body of the referenced unit.  This is used in the main generated
elaboration procedure by the binder and is not normally used in any
other context.  However, there may be specialized situations in which it
is useful to be able to call this elaboration procedure from Ada code,
e.g., if it is necessary to do selective re-elaboration to fix some
error.

Attribute Elab_Spec
===================
.. index:: Elab_Spec

This attribute can only be applied to a program unit name.  It returns
the entity for the corresponding elaboration procedure for elaborating
the spec of the referenced unit.  This is used in the main
generated elaboration procedure by the binder and is not normally used
in any other context.  However, there may be specialized situations in
which it is useful to be able to call this elaboration procedure from
Ada code, e.g., if it is necessary to do selective re-elaboration to fix
some error.

Attribute Elab_Subp_Body
========================
.. index:: Elab_Subp_Body

This attribute can only be applied to a library level subprogram
name and is only allowed in CodePeer mode. It returns the entity
for the corresponding elaboration procedure for elaborating the body
of the referenced subprogram unit. This is used in the main generated
elaboration procedure by the binder in CodePeer mode only and is unrecognized
otherwise.

Attribute Emax
==============
.. index:: Ada 83 attributes

.. index:: Emax

The ``Emax`` attribute is provided for compatibility with Ada 83.  See
the Ada 83 reference manual for an exact description of the semantics of
this attribute.

Attribute Enabled
=================
.. index:: Enabled

The ``Enabled`` attribute allows an application program to check at compile
time to see if the designated check is currently enabled. The prefix is a
simple identifier, referencing any predefined check name (other than
``All_Checks``) or a check name introduced by pragma Check_Name. If
no argument is given for the attribute, the check is for the general state
of the check, if an argument is given, then it is an entity name, and the
check indicates whether an ``Suppress`` or ``Unsuppress`` has been
given naming the entity (if not, then the argument is ignored).

Note that instantiations inherit the check status at the point of the
instantiation, so a useful idiom is to have a library package that
introduces a check name with ``pragma Check_Name``, and then contains
generic packages or subprograms which use the ``Enabled`` attribute
to see if the check is enabled. A user of this package can then issue
a ``pragma Suppress`` or ``pragma Unsuppress`` before instantiating
the package or subprogram, controlling whether the check will be present.

Attribute Enum_Rep
==================
.. index:: Representation of enums

.. index:: Enum_Rep

Note that this attribute is now standard in Ada 202x and is available
as an implementation defined attribute for earlier Ada versions.

For every enumeration subtype ``S``, ``S'Enum_Rep`` denotes a
function with the following spec:

.. code-block:: ada

  function S'Enum_Rep (Arg : S'Base) return <Universal_Integer>;


It is also allowable to apply ``Enum_Rep`` directly to an object of an
enumeration type or to a non-overloaded enumeration
literal.  In this case ``S'Enum_Rep`` is equivalent to
``typ'Enum_Rep(S)`` where ``typ`` is the type of the
enumeration literal or object.

The function returns the representation value for the given enumeration
value.  This will be equal to value of the ``Pos`` attribute in the
absence of an enumeration representation clause.  This is a static
attribute (i.e., the result is static if the argument is static).

``S'Enum_Rep`` can also be used with integer types and objects,
in which case it simply returns the integer value.  The reason for this
is to allow it to be used for ``(<>)`` discrete formal arguments in
a generic unit that can be instantiated with either enumeration types
or integer types.  Note that if ``Enum_Rep`` is used on a modular
type whose upper bound exceeds the upper bound of the largest signed
integer type, and the argument is a variable, so that the universal
integer calculation is done at run time, then the call to ``Enum_Rep``
may raise ``Constraint_Error``.

Attribute Enum_Val
==================
.. index:: Representation of enums

.. index:: Enum_Val

Note that this attribute is now standard in Ada 202x and is available
as an implementation defined attribute for earlier Ada versions.

For every enumeration subtype ``S``, ``S'Enum_Val`` denotes a
function with the following spec:

.. code-block:: ada

  function S'Enum_Val (Arg : <Universal_Integer>) return S'Base;


The function returns the enumeration value whose representation matches the
argument, or raises Constraint_Error if no enumeration literal of the type
has the matching value.
This will be equal to value of the ``Val`` attribute in the
absence of an enumeration representation clause.  This is a static
attribute (i.e., the result is static if the argument is static).

Attribute Epsilon
=================
.. index:: Ada 83 attributes

.. index:: Epsilon

The ``Epsilon`` attribute is provided for compatibility with Ada 83.  See
the Ada 83 reference manual for an exact description of the semantics of
this attribute.

Attribute Fast_Math
===================
.. index:: Fast_Math

``Standard'Fast_Math`` (``Standard`` is the only allowed
prefix) yields a static Boolean value that is True if pragma
``Fast_Math`` is active, and False otherwise.

Attribute Finalization_Size
===========================
.. index:: Finalization_Size

The prefix of attribute ``Finalization_Size`` must be an object or
a non-class-wide type. This attribute returns the size of any hidden data
reserved by the compiler to handle finalization-related actions. The type of
the attribute is *universal_integer*.

``Finalization_Size`` yields a value of zero for a type with no controlled
parts, an object whose type has no controlled parts, or an object of a
class-wide type whose tag denotes a type with no controlled parts.

Note that only heap-allocated objects contain finalization data.

Attribute Fixed_Value
=====================
.. index:: Fixed_Value

For every fixed-point type ``S``, ``S'Fixed_Value`` denotes a
function with the following specification:

.. code-block:: ada

  function S'Fixed_Value (Arg : <Universal_Integer>) return S;

The value returned is the fixed-point value ``V`` such that::

  V = Arg * S'Small


The effect is thus similar to first converting the argument to the
integer type used to represent ``S``, and then doing an unchecked
conversion to the fixed-point type.  The difference is
that there are full range checks, to ensure that the result is in range.
This attribute is primarily intended for use in implementation of the
input-output functions for fixed-point values.

Attribute From_Any
==================
.. index:: From_Any

This internal attribute is used for the generation of remote subprogram
stubs in the context of the Distributed Systems Annex.

Attribute Has_Access_Values
===========================
.. index:: Access values, testing for

.. index:: Has_Access_Values

The prefix of the ``Has_Access_Values`` attribute is a type.  The result
is a Boolean value which is True if the is an access type, or is a composite
type with a component (at any nesting depth) that is an access type, and is
False otherwise.
The intended use of this attribute is in conjunction with generic
definitions.  If the attribute is applied to a generic private type, it
indicates whether or not the corresponding actual type has access values.

Attribute Has_Discriminants
===========================
.. index:: Discriminants, testing for

.. index:: Has_Discriminants

The prefix of the ``Has_Discriminants`` attribute is a type.  The result
is a Boolean value which is True if the type has discriminants, and False
otherwise.  The intended use of this attribute is in conjunction with generic
definitions.  If the attribute is applied to a generic private type, it
indicates whether or not the corresponding actual type has discriminants.

Attribute Has_Tagged_Values
===========================
.. index:: Tagged values, testing for

.. index:: Has_Tagged_Values

The prefix of the ``Has_Tagged_Values`` attribute is a type. The result is a
Boolean value which is True if the type is a composite type (array or record)
that is either a tagged type or has a subcomponent that is tagged, and is False
otherwise. The intended use of this attribute is in conjunction with generic
definitions. If the attribute is applied to a generic private type, it
indicates whether or not the corresponding actual type has access values.

Attribute Img
=============
.. index:: Img

The ``Img`` attribute differs from ``Image`` in that, while both can be
applied directly to an object, ``Img`` cannot be applied to types.

Example usage of the attribute:

.. code-block:: ada

  Put_Line ("X = " & X'Img);


which has the same meaning as the more verbose:

.. code-block:: ada

  Put_Line ("X = " & T'Image (X));

where ``T`` is the (sub)type of the object ``X``.

Note that technically, in analogy to ``Image``,
``X'Img`` returns a parameterless function
that returns the appropriate string when called. This means that
``X'Img`` can be renamed as a function-returning-string, or used
in an instantiation as a function parameter.

Attribute Initialized
=====================
.. index:: Initialized

For the syntax and semantics of this attribute, see the SPARK 2014 Reference
Manual, section 6.10.

Attribute Integer_Value
=======================
.. index:: Integer_Value

For every integer type ``S``, ``S'Integer_Value`` denotes a
function with the following spec:

.. code-block:: ada

  function S'Integer_Value (Arg : <Universal_Fixed>) return S;

The value returned is the integer value ``V``, such that::

  Arg = V * T'Small


where ``T`` is the type of ``Arg``.
The effect is thus similar to first doing an unchecked conversion from
the fixed-point type to its corresponding implementation type, and then
converting the result to the target integer type.  The difference is
that there are full range checks, to ensure that the result is in range.
This attribute is primarily intended for use in implementation of the
standard input-output functions for fixed-point values.

Attribute Invalid_Value
=======================
.. index:: Invalid_Value

For every scalar type S, S'Invalid_Value returns an undefined value of the
type. If possible this value is an invalid representation for the type. The
value returned is identical to the value used to initialize an otherwise
uninitialized value of the type if pragma Initialize_Scalars is used,
including the ability to modify the value with the binder -Sxx flag and
relevant environment variables at run time.

Attribute Iterable
==================
.. index:: Iterable

Equivalent to Aspect Iterable.

Attribute Large
===============
.. index:: Ada 83 attributes

.. index:: Large

The ``Large`` attribute is provided for compatibility with Ada 83.  See
the Ada 83 reference manual for an exact description of the semantics of
this attribute.

Attribute Library_Level
=======================
.. index:: Library_Level

``P'Library_Level``, where P is an entity name,
returns a Boolean value which is True if the entity is declared
at the library level, and False otherwise. Note that within a
generic instantition, the name of the generic unit denotes the
instance, which means that this attribute can be used to test
if a generic is instantiated at the library level, as shown
in this example:

.. code-block:: ada

  generic
    ...
  package Gen is
    pragma Compile_Time_Error
      (not Gen'Library_Level,
       "Gen can only be instantiated at library level");
    ...
  end Gen;


Attribute Lock_Free
===================
.. index:: Lock_Free

``P'Lock_Free``, where P is a protected object, returns True if a
pragma ``Lock_Free`` applies to P.

Attribute Loop_Entry
====================
.. index:: Loop_Entry

Syntax::

  X'Loop_Entry [(loop_name)]


The ``Loop_Entry`` attribute is used to refer to the value that an
expression had upon entry to a given loop in much the same way that the
``Old`` attribute in a subprogram postcondition can be used to refer
to the value an expression had upon entry to the subprogram. The
relevant loop is either identified by the given loop name, or it is the
innermost enclosing loop when no loop name is given.

A ``Loop_Entry`` attribute can only occur within a
``Loop_Variant`` or ``Loop_Invariant`` pragma. A common use of
``Loop_Entry`` is to compare the current value of objects with their
initial value at loop entry, in a ``Loop_Invariant`` pragma.

The effect of using ``X'Loop_Entry`` is the same as declaring
a constant initialized with the initial value of ``X`` at loop
entry. This copy is not performed if the loop is not entered, or if the
corresponding pragmas are ignored or disabled.

Attribute Machine_Size
======================
.. index:: Machine_Size

This attribute is identical to the ``Object_Size`` attribute.  It is
provided for compatibility with the DEC Ada 83 attribute of this name.

Attribute Mantissa
==================
.. index:: Ada 83 attributes

.. index:: Mantissa

The ``Mantissa`` attribute is provided for compatibility with Ada 83.  See
the Ada 83 reference manual for an exact description of the semantics of
this attribute.

.. _Attribute_Maximum_Alignment:

Attribute Maximum_Alignment
===========================
.. index:: Alignment, maximum

.. index:: Maximum_Alignment

``Standard'Maximum_Alignment`` (``Standard`` is the only
allowed prefix) provides the maximum useful alignment value for the
target.  This is a static value that can be used to specify the alignment
for an object, guaranteeing that it is properly aligned in all
cases.

Attribute Max_Integer_Size
==========================
.. index:: Max_Integer_Size

``Standard'Max_Integer_Size`` (``Standard`` is the only allowed
prefix) provides the size of the largest supported integer type for
the target. The result is a static constant.

Attribute Mechanism_Code
========================
.. index:: Return values, passing mechanism

.. index:: Parameters, passing mechanism

.. index:: Mechanism_Code

``func'Mechanism_Code`` yields an integer code for the
mechanism used for the result of function ``func``, and
``subprog'Mechanism_Code (n)`` yields the mechanism
used for formal parameter number *n* (a static integer value, with 1
meaning the first parameter) of subprogram ``subprog``.  The code returned is:



*1*
  by copy (value)

*2*
  by reference

Attribute Null_Parameter
========================
.. index:: Zero address, passing

.. index:: Null_Parameter

A reference ``T'Null_Parameter`` denotes an imaginary object of
type or subtype ``T`` allocated at machine address zero.  The attribute
is allowed only as the default expression of a formal parameter, or as
an actual expression of a subprogram call.  In either case, the
subprogram must be imported.

The identity of the object is represented by the address zero in the
argument list, independent of the passing mechanism (explicit or
default).

This capability is needed to specify that a zero address should be
passed for a record or other composite object passed by reference.
There is no way of indicating this without the ``Null_Parameter``
attribute.

.. _Attribute-Object_Size:

Attribute Object_Size
=====================
.. index:: Size, used for objects

.. index:: Object_Size

The size of an object is not necessarily the same as the size of the type
of an object.  This is because by default object sizes are increased to be
a multiple of the alignment of the object.  For example,
``Natural'Size`` is
31, but by default objects of type ``Natural`` will have a size of 32 bits.
Similarly, a record containing an integer and a character:

.. code-block:: ada

  type Rec is record
     I : Integer;
     C : Character;
  end record;


will have a size of 40 (that is ``Rec'Size`` will be 40).  The
alignment will be 4, because of the
integer field, and so the default size of record objects for this type
will be 64 (8 bytes).

If the alignment of the above record is specified to be 1, then the
object size will be 40 (5 bytes). This is true by default, and also
an object size of 40 can be explicitly specified in this case.

A consequence of this capability is that different object sizes can be
given to subtypes that would otherwise be considered in Ada to be
statically matching.  But it makes no sense to consider such subtypes
as statically matching.  Consequently, GNAT adds a rule
to the static matching rules that requires object sizes to match.
Consider this example:

.. code-block:: ada

   1. procedure BadAVConvert is
   2.    type R is new Integer;
   3.    subtype R1 is R range 1 .. 10;
   4.    subtype R2 is R range 1 .. 10;
   5.    for R1'Object_Size use 8;
   6.    for R2'Object_Size use 16;
   7.    type R1P is access all R1;
   8.    type R2P is access all R2;
   9.    R1PV : R1P := new R1'(4);
  10.    R2PV : R2P;
  11. begin
  12.    R2PV := R2P (R1PV);
                 |
         >>> target designated subtype not compatible with
             type "R1" defined at line 3

  13. end;


In the absence of lines 5 and 6,
types ``R1`` and ``R2`` statically match and
hence the conversion on line 12 is legal. But since lines 5 and 6
cause the object sizes to differ, GNAT considers that types
``R1`` and ``R2`` are not statically matching, and line 12
generates the diagnostic shown above.

Similar additional checks are performed in other contexts requiring
statically matching subtypes.

Attribute Old
=============
.. index:: Old

In addition to the usage of ``Old`` defined in the Ada 2012 RM (usage
within ``Post`` aspect), GNAT also permits the use of this attribute
in implementation defined pragmas ``Postcondition``,
``Contract_Cases`` and ``Test_Case``. Also usages of
``Old`` which would be illegal according to the Ada 2012 RM
definition are allowed under control of
implementation defined pragma ``Unevaluated_Use_Of_Old``.

Attribute Passed_By_Reference
=============================
.. index:: Parameters, when passed by reference

.. index:: Passed_By_Reference

``typ'Passed_By_Reference`` for any subtype `typ` returns
a value of type ``Boolean`` value that is ``True`` if the type is
normally passed by reference and ``False`` if the type is normally
passed by copy in calls.  For scalar types, the result is always ``False``
and is static.  For non-scalar types, the result is nonstatic.

Attribute Pool_Address
======================
.. index:: Pool_Address

``X'Pool_Address`` for any object ``X`` returns the address
of X within its storage pool. This is the same as
``X'Address``, except that for an unconstrained array whose
bounds are allocated just before the first component,
``X'Pool_Address`` returns the address of those bounds,
whereas ``X'Address`` returns the address of the first
component.

Here, we are interpreting 'storage pool' broadly to mean
``wherever the object is allocated``, which could be a
user-defined storage pool,
the global heap, on the stack, or in a static memory area.
For an object created by ``new``, ``Ptr.all'Pool_Address`` is
what is passed to ``Allocate`` and returned from ``Deallocate``.

Attribute Range_Length
======================
.. index:: Range_Length

``typ'Range_Length`` for any discrete type `typ` yields
the number of values represented by the subtype (zero for a null
range).  The result is static for static subtypes.  ``Range_Length``
applied to the index subtype of a one dimensional array always gives the
same result as ``Length`` applied to the array itself.

Attribute Restriction_Set
=========================
.. index:: Restriction_Set
.. index:: Restrictions

This attribute allows compile time testing of restrictions that
are currently in effect. It is primarily intended for specializing
code in the run-time based on restrictions that are active (e.g.
don't need to save fpt registers if restriction No_Floating_Point
is known to be in effect), but can be used anywhere.

There are two forms:

.. code-block:: ada

  System'Restriction_Set (partition_boolean_restriction_NAME)
  System'Restriction_Set (No_Dependence => library_unit_NAME);


In the case of the first form, the only restriction names
allowed are parameterless restrictions that are checked
for consistency at bind time. For a complete list see the
subtype ``System.Rident.Partition_Boolean_Restrictions``.

The result returned is True if the restriction is known to
be in effect, and False if the restriction is known not to
be in effect. An important guarantee is that the value of
a Restriction_Set attribute is known to be consistent throughout
all the code of a partition.

This is trivially achieved if the entire partition is compiled
with a consistent set of restriction pragmas. However, the
compilation model does not require this. It is possible to
compile one set of units with one set of pragmas, and another
set of units with another set of pragmas. It is even possible
to compile a spec with one set of pragmas, and then WITH the
same spec with a different set of pragmas. Inconsistencies
in the actual use of the restriction are checked at bind time.

In order to achieve the guarantee of consistency for the
Restriction_Set pragma, we consider that a use of the pragma
that yields False is equivalent to a violation of the
restriction.

So for example if you write

.. code-block:: ada

  if System'Restriction_Set (No_Floating_Point) then
     ...
  else
     ...
  end if;


And the result is False, so that the else branch is executed,
you can assume that this restriction is not set for any unit
in the partition. This is checked by considering this use of
the restriction pragma to be a violation of the restriction
No_Floating_Point. This means that no other unit can attempt
to set this restriction (if some unit does attempt to set it,
the binder will refuse to bind the partition).

Technical note: The restriction name and the unit name are
intepreted entirely syntactically, as in the corresponding
Restrictions pragma, they are not analyzed semantically,
so they do not have a type.

Attribute Result
================
.. index:: Result

``function'Result`` can only be used with in a Postcondition pragma
for a function. The prefix must be the name of the corresponding function. This
is used to refer to the result of the function in the postcondition expression.
For a further discussion of the use of this attribute and examples of its use,
see the description of pragma Postcondition.

Attribute Safe_Emax
===================
.. index:: Ada 83 attributes

.. index:: Safe_Emax

The ``Safe_Emax`` attribute is provided for compatibility with Ada 83.  See
the Ada 83 reference manual for an exact description of the semantics of
this attribute.

Attribute Safe_Large
====================
.. index:: Ada 83 attributes

.. index:: Safe_Large

The ``Safe_Large`` attribute is provided for compatibility with Ada 83.  See
the Ada 83 reference manual for an exact description of the semantics of
this attribute.

Attribute Safe_Small
====================
.. index:: Ada 83 attributes

.. index:: Safe_Small

The ``Safe_Small`` attribute is provided for compatibility with Ada 83.  See
the Ada 83 reference manual for an exact description of the semantics of
this attribute.

.. _Attribute-Scalar_Storage_Order:

Attribute Scalar_Storage_Order
==============================
.. index:: Endianness

.. index:: Scalar storage order

.. index:: Scalar_Storage_Order

For every array or record type ``S``, the representation attribute
``Scalar_Storage_Order`` denotes the order in which storage elements
that make up scalar components are ordered within S. The value given must
be a static expression of type System.Bit_Order. The following is an example
of the use of this feature:

.. code-block:: ada

     --  Component type definitions

     subtype Yr_Type is Natural range 0 .. 127;
     subtype Mo_Type is Natural range 1 .. 12;
     subtype Da_Type is Natural range 1 .. 31;

     --  Record declaration

     type Date is record
        Years_Since_1980 : Yr_Type;
        Month            : Mo_Type;
        Day_Of_Month     : Da_Type;
     end record;

     --  Record representation clause

     for Date use record
        Years_Since_1980 at 0 range 0  ..  6;
        Month            at 0 range 7  .. 10;
        Day_Of_Month     at 0 range 11 .. 15;
     end record;

     --  Attribute definition clauses

     for Date'Bit_Order use System.High_Order_First;
     for Date'Scalar_Storage_Order use System.High_Order_First;
     --  If Scalar_Storage_Order is specified, it must be consistent with
     --  Bit_Order, so it's best to always define the latter explicitly if
     --  the former is used.


Other properties are as for the standard representation attribute ``Bit_Order``
defined by Ada RM 13.5.3(4). The default is ``System.Default_Bit_Order``.

For a record type ``T``, if ``T'Scalar_Storage_Order`` is
specified explicitly, it shall be equal to ``T'Bit_Order``. Note:
this means that if a ``Scalar_Storage_Order`` attribute definition
clause is not confirming, then the type's ``Bit_Order`` shall be
specified explicitly and set to the same value.

Derived types inherit an explicitly set scalar storage order from their parent
types. This may be overridden for the derived type by giving an explicit scalar
storage order for it. However, for a record extension, the derived type must
have the same scalar storage order as the parent type.

A component of a record type that is itself a record or an array and that does
not start and end on a byte boundary must have have the same scalar storage
order as the record type. A component of a bit-packed array type that is itself
a record or an array must have the same scalar storage order as the array type.

No component of a type that has an explicit ``Scalar_Storage_Order``
attribute definition may be aliased.

A confirming ``Scalar_Storage_Order`` attribute definition clause (i.e.
with a value equal to ``System.Default_Bit_Order``) has no effect.

If the opposite storage order is specified, then whenever the value of
a scalar component of an object of type ``S`` is read, the storage
elements of the enclosing machine scalar are first reversed (before
retrieving the component value, possibly applying some shift and mask
operatings on the enclosing machine scalar), and the opposite operation
is done for writes.

In that case, the restrictions set forth in 13.5.1(10.3/2) for scalar components
are relaxed. Instead, the following rules apply:

* the underlying storage elements are those at positions
  ``(position + first_bit / storage_element_size) .. (position + (last_bit + storage_element_size - 1) / storage_element_size)``
* the sequence of underlying storage elements shall have
  a size no greater than the largest machine scalar
* the enclosing machine scalar is defined as the smallest machine
  scalar starting at a position no greater than
  ``position + first_bit / storage_element_size`` and covering
  storage elements at least up to ``position + (last_bit + storage_element_size - 1) / storage_element_size```
* the position of the component is interpreted relative to that machine
  scalar.

If no scalar storage order is specified for a type (either directly, or by
inheritance in the case of a derived type), then the default is normally
the native ordering of the target, but this default can be overridden using
pragma ``Default_Scalar_Storage_Order``.

If a component of ``T`` is itself of a record or array type, the specfied
``Scalar_Storage_Order`` does *not* apply to that nested type: an explicit
attribute definition clause must be provided for the component type as well
if desired.

Note that the scalar storage order only affects the in-memory data
representation. It has no effect on the representation used by stream
attributes.

Note that debuggers may be unable to display the correct value of scalar
components of a type for which the opposite storage order is specified.

.. _Attribute_Simple_Storage_Pool:

Attribute Simple_Storage_Pool
=============================
.. index:: Storage pool, simple

.. index:: Simple storage pool

.. index:: Simple_Storage_Pool

For every nonformal, nonderived access-to-object type ``Acc``, the
representation attribute ``Simple_Storage_Pool`` may be specified
via an attribute_definition_clause (or by specifying the equivalent aspect):

.. code-block:: ada

  My_Pool : My_Simple_Storage_Pool_Type;

  type Acc is access My_Data_Type;

  for Acc'Simple_Storage_Pool use My_Pool;



The name given in an attribute_definition_clause for the
``Simple_Storage_Pool`` attribute shall denote a variable of
a 'simple storage pool type' (see pragma `Simple_Storage_Pool_Type`).

The use of this attribute is only allowed for a prefix denoting a type
for which it has been specified. The type of the attribute is the type
of the variable specified as the simple storage pool of the access type,
and the attribute denotes that variable.

It is illegal to specify both ``Storage_Pool`` and ``Simple_Storage_Pool``
for the same access type.

If the ``Simple_Storage_Pool`` attribute has been specified for an access
type, then applying the ``Storage_Pool`` attribute to the type is flagged
with a warning and its evaluation raises the exception ``Program_Error``.

If the Simple_Storage_Pool attribute has been specified for an access
type ``S``, then the evaluation of the attribute ``S'Storage_Size``
returns the result of calling ``Storage_Size (S'Simple_Storage_Pool)``,
which is intended to indicate the number of storage elements reserved for
the simple storage pool. If the Storage_Size function has not been defined
for the simple storage pool type, then this attribute returns zero.

If an access type ``S`` has a specified simple storage pool of type
``SSP``, then the evaluation of an allocator for that access type calls
the primitive ``Allocate`` procedure for type ``SSP``, passing
``S'Simple_Storage_Pool`` as the pool parameter. The detailed
semantics of such allocators is the same as those defined for allocators
in section 13.11 of the :title:`Ada Reference Manual`, with the term
*simple storage pool* substituted for *storage pool*.

If an access type ``S`` has a specified simple storage pool of type
``SSP``, then a call to an instance of the ``Ada.Unchecked_Deallocation``
for that access type invokes the primitive ``Deallocate`` procedure
for type ``SSP``, passing ``S'Simple_Storage_Pool`` as the pool
parameter. The detailed semantics of such unchecked deallocations is the same
as defined in section 13.11.2 of the Ada Reference Manual, except that the
term *simple storage pool* is substituted for *storage pool*.

Attribute Small
===============
.. index:: Ada 83 attributes

.. index:: Small

The ``Small`` attribute is defined in Ada 95 (and Ada 2005) only for
fixed-point types.
GNAT also allows this attribute to be applied to floating-point types
for compatibility with Ada 83.  See
the Ada 83 reference manual for an exact description of the semantics of
this attribute when applied to floating-point types.

Attribute Small_Denominator
===========================
.. index:: Small

.. index:: Small_Denominator

``typ'Small_Denominator`` for any fixed-point subtype `typ` yields the
denominator in the representation of ``typ'Small`` as a rational number
with coprime factors (i.e. as an irreducible fraction).

Attribute Small_Numerator
=========================
.. index:: Small

.. index:: Small_Numerator

``typ'Small_Numerator`` for any fixed-point subtype `typ` yields the
numerator in the representation of ``typ'Small`` as a rational number
with coprime factors (i.e. as an irreducible fraction).

Attribute Storage_Unit
======================
.. index:: Storage_Unit

``Standard'Storage_Unit`` (``Standard`` is the only allowed
prefix) provides the same value as ``System.Storage_Unit``.

Attribute Stub_Type
===================
.. index:: Stub_Type

The GNAT implementation of remote access-to-classwide types is
organized as described in AARM section E.4 (20.t): a value of an RACW type
(designating a remote object) is represented as a normal access
value, pointing to a "stub" object which in turn contains the
necessary information to contact the designated remote object. A
call on any dispatching operation of such a stub object does the
remote call, if necessary, using the information in the stub object
to locate the target partition, etc.

For a prefix ``T`` that denotes a remote access-to-classwide type,
``T'Stub_Type`` denotes the type of the corresponding stub objects.

By construction, the layout of ``T'Stub_Type`` is identical to that of
type ``RACW_Stub_Type`` declared in the internal implementation-defined
unit ``System.Partition_Interface``. Use of this attribute will create
an implicit dependency on this unit.

Attribute System_Allocator_Alignment
====================================
.. index:: Alignment, allocator

.. index:: System_Allocator_Alignment

``Standard'System_Allocator_Alignment`` (``Standard`` is the only
allowed prefix) provides the observable guaranted to be honored by
the system allocator (malloc). This is a static value that can be used
in user storage pools based on malloc either to reject allocation
with alignment too large or to enable a realignment circuitry if the
alignment request is larger than this value.

Attribute Target_Name
=====================
.. index:: Target_Name

``Standard'Target_Name`` (``Standard`` is the only allowed
prefix) provides a static string value that identifies the target
for the current compilation. For GCC implementations, this is the
standard gcc target name without the terminating slash (for
example, GNAT 5.0 on windows yields "i586-pc-mingw32msv").

Attribute To_Address
====================
.. index:: To_Address

The ``System'To_Address``
(``System`` is the only allowed prefix)
denotes a function identical to
``System.Storage_Elements.To_Address`` except that
it is a static attribute.  This means that if its argument is
a static expression, then the result of the attribute is a
static expression.  This means that such an expression can be
used in contexts (e.g., preelaborable packages) which require a
static expression and where the function call could not be used
(since the function call is always nonstatic, even if its
argument is static). The argument must be in the range
-(2**(m-1)) .. 2**m-1, where m is the memory size
(typically 32 or 64). Negative values are intepreted in a
modular manner (e.g., -1 means the same as 16#FFFF_FFFF# on
a 32 bits machine).

Attribute To_Any
================
.. index:: To_Any

This internal attribute is used for the generation of remote subprogram
stubs in the context of the Distributed Systems Annex.

Attribute Type_Class
====================
.. index:: Type_Class

``typ'Type_Class`` for any type or subtype `typ` yields
the value of the type class for the full type of `typ`.  If
`typ` is a generic formal type, the value is the value for the
corresponding actual subtype.  The value of this attribute is of type
``System.Aux_DEC.Type_Class``, which has the following definition:

.. code-block:: ada

  type Type_Class is
    (Type_Class_Enumeration,
     Type_Class_Integer,
     Type_Class_Fixed_Point,
     Type_Class_Floating_Point,
     Type_Class_Array,
     Type_Class_Record,
     Type_Class_Access,
     Type_Class_Task,
     Type_Class_Address);


Protected types yield the value ``Type_Class_Task``, which thus
applies to all concurrent types.  This attribute is designed to
be compatible with the DEC Ada 83 attribute of the same name.

Attribute Type_Key
==================
.. index:: Type_Key

The ``Type_Key`` attribute is applicable to a type or subtype and
yields a value of type Standard.String containing encoded information
about the type or subtype. This provides improved compatibility with
other implementations that support this attribute.

Attribute TypeCode
==================
.. index:: TypeCode

This internal attribute is used for the generation of remote subprogram
stubs in the context of the Distributed Systems Annex.

Attribute Unconstrained_Array
=============================
.. index:: Unconstrained_Array

The ``Unconstrained_Array`` attribute can be used with a prefix that
denotes any type or subtype. It is a static attribute that yields
``True`` if the prefix designates an unconstrained array,
and ``False`` otherwise. In a generic instance, the result is
still static, and yields the result of applying this test to the
generic actual.

Attribute Universal_Literal_String
==================================
.. index:: Named numbers, representation of

.. index:: Universal_Literal_String

The prefix of ``Universal_Literal_String`` must be a named
number.  The static result is the string consisting of the characters of
the number as defined in the original source.  This allows the user
program to access the actual text of named numbers without intermediate
conversions and without the need to enclose the strings in quotes (which
would preclude their use as numbers).

For example, the following program prints the first 50 digits of pi:

.. code-block:: ada

  with Text_IO; use Text_IO;
  with Ada.Numerics;
  procedure Pi is
  begin
     Put (Ada.Numerics.Pi'Universal_Literal_String);
  end;


Attribute Unrestricted_Access
=============================
.. index:: Access, unrestricted

.. index:: Unrestricted_Access

The ``Unrestricted_Access`` attribute is similar to ``Access``
except that all accessibility and aliased view checks are omitted.  This
is a user-beware attribute.

For objects, it is similar to ``Address``, for which it is a
desirable replacement where the value desired is an access type.
In other words, its effect is similar to first applying the
``Address`` attribute and then doing an unchecked conversion to a
desired access type.

For subprograms, ``P'Unrestricted_Access`` may be used where
``P'Access`` would be illegal, to construct a value of a
less-nested named access type that designates a more-nested
subprogram. This value may be used in indirect calls, so long as the
more-nested subprogram still exists; once the subprogram containing it
has returned, such calls are erroneous. For example:

.. code-block:: ada

  package body P is

     type Less_Nested is not null access procedure;
     Global : Less_Nested;

     procedure P1 is
     begin
        Global.all;
     end P1;

     procedure P2 is
        Local_Var : Integer;

        procedure More_Nested is
        begin
           ... Local_Var ...
        end More_Nested;
     begin
        Global := More_Nested'Unrestricted_Access;
        P1;
     end P2;

  end P;


When P1 is called from P2, the call via Global is OK, but if P1 were
called after P2 returns, it would be an erroneous use of a dangling
pointer.

For objects, it is possible to use ``Unrestricted_Access`` for any
type. However, if the result is of an access-to-unconstrained array
subtype, then the resulting pointer has the same scope as the context
of the attribute, and must not be returned to some enclosing scope.
For instance, if a function uses ``Unrestricted_Access`` to create
an access-to-unconstrained-array and returns that value to the caller,
the result will involve dangling pointers. In addition, it is only
valid to create pointers to unconstrained arrays using this attribute
if the pointer has the normal default 'fat' representation where a
pointer has two components, one points to the array and one points to
the bounds. If a size clause is used to force 'thin' representation
for a pointer to unconstrained where there is only space for a single
pointer, then the resulting pointer is not usable.

In the simple case where a direct use of Unrestricted_Access attempts
to make a thin pointer for a non-aliased object, the compiler will
reject the use as illegal, as shown in the following example:

.. code-block:: ada

  with System; use System;
  procedure SliceUA2 is
     type A is access all String;
     for A'Size use Standard'Address_Size;

     procedure P (Arg : A) is
     begin
        null;
     end P;

     X : String := "hello world!";
     X2 : aliased String := "hello world!";

     AV : A := X'Unrestricted_Access;    -- ERROR
               |
  >>> illegal use of Unrestricted_Access attribute
  >>> attempt to generate thin pointer to unaliased object

  begin
     P (X'Unrestricted_Access);          -- ERROR
        |
  >>> illegal use of Unrestricted_Access attribute
  >>> attempt to generate thin pointer to unaliased object

     P (X(7 .. 12)'Unrestricted_Access); -- ERROR
        |
  >>> illegal use of Unrestricted_Access attribute
  >>> attempt to generate thin pointer to unaliased object

     P (X2'Unrestricted_Access);         -- OK
  end;


but other cases cannot be detected by the compiler, and are
considered to be erroneous. Consider the following example:

.. code-block:: ada

  with System; use System;
  with System; use System;
  procedure SliceUA is
     type AF is access all String;

     type A is access all String;
     for A'Size use Standard'Address_Size;

     procedure P (Arg : A) is
     begin
        if Arg'Length /= 6 then
           raise Program_Error;
        end if;
     end P;

     X : String := "hello world!";
     Y : AF := X (7 .. 12)'Unrestricted_Access;

  begin
     P (A (Y));
  end;


A normal unconstrained array value
or a constrained array object marked as aliased has the bounds in memory
just before the array, so a thin pointer can retrieve both the data and
the bounds.  But in this case, the non-aliased object ``X`` does not have the
bounds before the string.  If the size clause for type ``A``
were not present, then the pointer
would be a fat pointer, where one component is a pointer to the bounds,
and all would be well.  But with the size clause present, the conversion from
fat pointer to thin pointer in the call loses the bounds, and so this
is erroneous, and the program likely raises a ``Program_Error`` exception.

In general, it is advisable to completely
avoid mixing the use of thin pointers and the use of
``Unrestricted_Access`` where the designated type is an
unconstrained array.  The use of thin pointers should be restricted to
cases of porting legacy code that implicitly assumes the size of pointers,
and such code should not in any case be using this attribute.

Another erroneous situation arises if the attribute is
applied to a constant. The resulting pointer can be used to access the
constant, but the effect of trying to modify a constant in this manner
is not well-defined. Consider this example:

.. code-block:: ada

  P : constant Integer := 4;
  type R is access all Integer;
  RV : R := P'Unrestricted_Access;
  ..
  RV.all := 3;


Here we attempt to modify the constant P from 4 to 3, but the compiler may
or may not notice this attempt, and subsequent references to P may yield
either the value 3 or the value 4 or the assignment may blow up if the
compiler decides to put P in read-only memory. One particular case where
``Unrestricted_Access`` can be used in this way is to modify the
value of an ``in`` parameter:

.. code-block:: ada

  procedure K (S : in String) is
     type R is access all Character;
     RV : R := S (3)'Unrestricted_Access;
  begin
     RV.all := 'a';
  end;


In general this is a risky approach. It may appear to "work" but such uses of
``Unrestricted_Access`` are potentially non-portable, even from one version
of GNAT to another, so are best avoided if possible.

Attribute Update
================
.. index:: Update

The ``Update`` attribute creates a copy of an array or record value
with one or more modified components. The syntax is::

  PREFIX'Update ( RECORD_COMPONENT_ASSOCIATION_LIST )
  PREFIX'Update ( ARRAY_COMPONENT_ASSOCIATION {, ARRAY_COMPONENT_ASSOCIATION } )
  PREFIX'Update ( MULTIDIMENSIONAL_ARRAY_COMPONENT_ASSOCIATION
                  {, MULTIDIMENSIONAL_ARRAY_COMPONENT_ASSOCIATION } )

  MULTIDIMENSIONAL_ARRAY_COMPONENT_ASSOCIATION ::= INDEX_EXPRESSION_LIST_LIST => EXPRESSION
  INDEX_EXPRESSION_LIST_LIST                   ::= INDEX_EXPRESSION_LIST {| INDEX_EXPRESSION_LIST }
  INDEX_EXPRESSION_LIST                        ::= ( EXPRESSION {, EXPRESSION } )


where ``PREFIX`` is the name of an array or record object, the
association list in parentheses does not contain an ``others``
choice and the box symbol ``<>`` may not appear in any
expression. The effect is to yield a copy of the array or record value
which is unchanged apart from the components mentioned in the
association list, which are changed to the indicated value. The
original value of the array or record value is not affected. For
example:

.. code-block:: ada

  type Arr is Array (1 .. 5) of Integer;
  ...
  Avar1 : Arr := (1,2,3,4,5);
  Avar2 : Arr := Avar1'Update (2 => 10, 3 .. 4 => 20);


yields a value for ``Avar2`` of 1,10,20,20,5 with ``Avar1``
begin unmodified. Similarly:

.. code-block:: ada

  type Rec is A, B, C : Integer;
  ...
  Rvar1 : Rec := (A => 1, B => 2, C => 3);
  Rvar2 : Rec := Rvar1'Update (B => 20);


yields a value for ``Rvar2`` of (A => 1, B => 20, C => 3),
with ``Rvar1`` being unmodifed.
Note that the value of the attribute reference is computed
completely before it is used. This means that if you write:

.. code-block:: ada

  Avar1 := Avar1'Update (1 => 10, 2 => Function_Call);


then the value of ``Avar1`` is not modified if ``Function_Call``
raises an exception, unlike the effect of a series of direct assignments
to elements of ``Avar1``. In general this requires that
two extra complete copies of the object are required, which should be
kept in mind when considering efficiency.

The ``Update`` attribute cannot be applied to prefixes of a limited
type, and cannot reference discriminants in the case of a record type.
The accessibility level of an Update attribute result object is defined
as for an aggregate.

In the record case, no component can be mentioned more than once. In
the array case, two overlapping ranges can appear in the association list,
in which case the modifications are processed left to right.

Multi-dimensional arrays can be modified, as shown by this example:

.. code-block:: ada

  A : array (1 .. 10, 1 .. 10) of Integer;
  ..
  A := A'Update ((1, 2) => 20, (3, 4) => 30);


which changes element (1,2) to 20 and (3,4) to 30.

Attribute Valid_Scalars
=======================
.. index:: Valid_Scalars

The ``'Valid_Scalars`` attribute is intended to make it easier to check the
validity of scalar subcomponents of composite objects. The attribute is defined
for any prefix ``P`` which denotes an object. Prefix ``P`` can be any type
except for tagged private or ``Unchecked_Union`` types. The value of the
attribute is of type ``Boolean``.

``P'Valid_Scalars`` yields ``True`` if and only if the evaluation of
``C'Valid`` yields ``True`` for every scalar subcomponent ``C`` of ``P``, or if
``P`` has no scalar subcomponents. Attribute ``'Valid_Scalars`` is equivalent
to attribute ``'Valid`` for scalar types.

It is not specified in what order the subcomponents are checked, nor whether
any more are checked after any one of them is determined to be invalid. If the
prefix ``P`` is of a class-wide type ``T'Class`` (where ``T`` is the associated
specific type), or if the prefix ``P`` is of a specific tagged type ``T``, then
only the subcomponents of ``T`` are checked; in other words, components of
extensions of ``T`` are not checked even if ``T'Class (P)'Tag /= T'Tag``.

The compiler will issue a warning if it can be determined at compile time that
the prefix of the attribute has no scalar subcomponents.

Note: ``Valid_Scalars`` can generate a lot of code, especially in the case of
a large variant record. If the attribute is called in many places in the same
program applied to objects of the same type, it can reduce program size to
write a function with a single use of the attribute, and then call that
function from multiple places.

Attribute VADS_Size
===================
.. index:: Size, VADS compatibility

.. index:: VADS_Size

The ``'VADS_Size`` attribute is intended to make it easier to port
legacy code which relies on the semantics of ``'Size`` as implemented
by the VADS Ada 83 compiler.  GNAT makes a best effort at duplicating the
same semantic interpretation.  In particular, ``'VADS_Size`` applied
to a predefined or other primitive type with no Size clause yields the
Object_Size (for example, ``Natural'Size`` is 32 rather than 31 on
typical machines).  In addition ``'VADS_Size`` applied to an object
gives the result that would be obtained by applying the attribute to
the corresponding type.

.. _Attribute-Value_Size:

Attribute Value_Size
====================
.. index:: Size, setting for not-first subtype

.. index:: Value_Size

``type'Value_Size`` is the number of bits required to represent
a value of the given subtype.  It is the same as ``type'Size``,
but, unlike ``Size``, may be set for non-first subtypes.

Attribute Wchar_T_Size
======================
.. index:: Wchar_T_Size

``Standard'Wchar_T_Size`` (``Standard`` is the only allowed
prefix) provides the size in bits of the C ``wchar_t`` type
primarily for constructing the definition of this type in
package ``Interfaces.C``. The result is a static constant.

Attribute Word_Size
===================
.. index:: Word_Size

``Standard'Word_Size`` (``Standard`` is the only allowed
prefix) provides the value ``System.Word_Size``. The result is
a static constant.
