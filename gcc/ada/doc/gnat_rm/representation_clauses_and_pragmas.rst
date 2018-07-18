.. role:: switch(samp)

.. _Representation_Clauses_and_Pragmas:

**********************************
Representation Clauses and Pragmas
**********************************

.. index:: Representation Clauses

.. index:: Representation Clause

.. index:: Representation Pragma

.. index:: Pragma, representation

This section describes the representation clauses accepted by GNAT, and
their effect on the representation of corresponding data objects.

GNAT fully implements Annex C (Systems Programming).  This means that all
the implementation advice sections in chapter 13 are fully implemented.
However, these sections only require a minimal level of support for
representation clauses.  GNAT provides much more extensive capabilities,
and this section describes the additional capabilities provided.

.. _Alignment_Clauses:

Alignment Clauses
=================

.. index:: Alignment Clause

GNAT requires that all alignment clauses specify a power of 2, and all
default alignments are always a power of 2.  The default alignment
values are as follows:

* *Elementary Types*.

  For elementary types, the alignment is the minimum of the actual size of
  objects of the type divided by ``Storage_Unit``,
  and the maximum alignment supported by the target.
  (This maximum alignment is given by the GNAT-specific attribute
  ``Standard'Maximum_Alignment``; see :ref:`Attribute_Maximum_Alignment`.)

  .. index:: Maximum_Alignment attribute

  For example, for type ``Long_Float``, the object size is 8 bytes, and the
  default alignment will be 8 on any target that supports alignments
  this large, but on some targets, the maximum alignment may be smaller
  than 8, in which case objects of type ``Long_Float`` will be maximally
  aligned.

* *Arrays*.

  For arrays, the alignment is equal to the alignment of the component type
  for the normal case where no packing or component size is given.  If the
  array is packed, and the packing is effective (see separate section on
  packed arrays), then the alignment will be either 4, 2, or 1 for long packed
  arrays or arrays whose length is not known at compile time, depending on
  whether the component size is divisible by 4, 2, or is odd.  For short packed
  arrays, which are handled internally as modular types, the alignment
  will be as described for elementary types, e.g. a packed array of length
  31 bits will have an object size of four bytes, and an alignment of 4.

* *Records*.

  For the normal unpacked case, the alignment of a record is equal to
  the maximum alignment of any of its components.  For tagged records, this
  includes the implicit access type used for the tag.  If a pragma ``Pack``
  is used and all components are packable (see separate section on pragma
  ``Pack``), then the resulting alignment is 1, unless the layout of the
  record makes it profitable to increase it.

  A special case is when:

  * the size of the record is given explicitly, or a
    full record representation clause is given, and

  * the size of the record is 2, 4, or 8 bytes.

  In this case, an alignment is chosen to match the
  size of the record. For example, if we have:

  .. code-block:: ada

       type Small is record
          A, B : Character;
       end record;
       for Small'Size use 16;

  then the default alignment of the record type ``Small`` is 2, not 1. This
  leads to more efficient code when the record is treated as a unit, and also
  allows the type to specified as ``Atomic`` on architectures requiring
  strict alignment.

An alignment clause may specify a larger alignment than the default value
up to some maximum value dependent on the target (obtainable by using the
attribute reference ``Standard'Maximum_Alignment``). It may also specify
a smaller alignment than the default value for enumeration, integer and
fixed point types, as well as for record types, for example

.. code-block:: ada

    type V is record
       A : Integer;
    end record;

    for V'alignment use 1;

.. index:: Alignment, default

The default alignment for the type ``V`` is 4, as a result of the
Integer field in the record, but it is permissible, as shown, to
override the default alignment of the record with a smaller value.

.. index:: Alignment, subtypes

Note that according to the Ada standard, an alignment clause applies only
to the first named subtype. If additional subtypes are declared, then the
compiler is allowed to choose any alignment it likes, and there is no way
to control this choice. Consider:

.. code-block:: ada

     type R is range 1 .. 10_000;
     for R'Alignment use 1;
     subtype RS is R range 1 .. 1000;

The alignment clause specifies an alignment of 1 for the first named subtype
``R`` but this does not necessarily apply to ``RS``. When writing
portable Ada code, you should avoid writing code that explicitly or
implicitly relies on the alignment of such subtypes.

For the GNAT compiler, if an explicit alignment clause is given, this
value is also used for any subsequent subtypes. So for GNAT, in the
above example, you can count on the alignment of ``RS`` being 1. But this
assumption is non-portable, and other compilers may choose different
alignments for the subtype ``RS``.

.. _Size_Clauses:

Size Clauses
============

.. index:: Size Clause

The default size for a type ``T`` is obtainable through the
language-defined attribute ``T'Size`` and also through the
equivalent GNAT-defined attribute ``T'Value_Size``.
For objects of type ``T``, GNAT will generally increase the type size
so that the object size (obtainable through the GNAT-defined attribute
``T'Object_Size``)
is a multiple of ``T'Alignment * Storage_Unit``.

For example:

.. code-block:: ada

     type Smallint is range 1 .. 6;

     type Rec is record
        Y1 : integer;
        Y2 : boolean;
     end record;

In this example, ``Smallint'Size`` = ``Smallint'Value_Size`` = 3,
as specified by the RM rules,
but objects of this type will have a size of 8
(``Smallint'Object_Size`` = 8),
since objects by default occupy an integral number
of storage units.  On some targets, notably older
versions of the Digital Alpha, the size of stand
alone objects of this type may be 32, reflecting
the inability of the hardware to do byte load/stores.

Similarly, the size of type ``Rec`` is 40 bits
(``Rec'Size`` = ``Rec'Value_Size`` = 40), but
the alignment is 4, so objects of this type will have
their size increased to 64 bits so that it is a multiple
of the alignment (in bits).  This decision is
in accordance with the specific Implementation Advice in RM 13.3(43):

   "A ``Size`` clause should be supported for an object if the specified
   ``Size`` is at least as large as its subtype's ``Size``, and corresponds
   to a size in storage elements that is a multiple of the object's
   ``Alignment`` (if the ``Alignment`` is nonzero)."

An explicit size clause may be used to override the default size by
increasing it.  For example, if we have:

.. code-block:: ada

     type My_Boolean is new Boolean;
     for My_Boolean'Size use 32;

then values of this type will always be 32 bits long.  In the case of
discrete types, the size can be increased up to 64 bits, with the effect
that the entire specified field is used to hold the value, sign- or
zero-extended as appropriate.  If more than 64 bits is specified, then
padding space is allocated after the value, and a warning is issued that
there are unused bits.

Similarly the size of records and arrays may be increased, and the effect
is to add padding bits after the value.  This also causes a warning message
to be generated.

The largest Size value permitted in GNAT is 2**31-1.  Since this is a
Size in bits, this corresponds to an object of size 256 megabytes (minus
one).  This limitation is true on all targets.  The reason for this
limitation is that it improves the quality of the code in many cases
if it is known that a Size value can be accommodated in an object of
type Integer.


.. _Storage_Size_Clauses:

Storage_Size Clauses
====================

.. index:: Storage_Size Clause

For tasks, the ``Storage_Size`` clause specifies the amount of space
to be allocated for the task stack.  This cannot be extended, and if the
stack is exhausted, then ``Storage_Error`` will be raised (if stack
checking is enabled).  Use a ``Storage_Size`` attribute definition clause,
or a ``Storage_Size`` pragma in the task definition to set the
appropriate required size.  A useful technique is to include in every
task definition a pragma of the form:

.. code-block:: ada

     pragma Storage_Size (Default_Stack_Size);

Then ``Default_Stack_Size`` can be defined in a global package, and
modified as required. Any tasks requiring stack sizes different from the
default can have an appropriate alternative reference in the pragma.

You can also use the *-d* binder switch to modify the default stack
size.

For access types, the ``Storage_Size`` clause specifies the maximum
space available for allocation of objects of the type.  If this space is
exceeded then ``Storage_Error`` will be raised by an allocation attempt.
In the case where the access type is declared local to a subprogram, the
use of a ``Storage_Size`` clause triggers automatic use of a special
predefined storage pool (``System.Pool_Size``) that ensures that all
space for the pool is automatically reclaimed on exit from the scope in
which the type is declared.

A special case recognized by the compiler is the specification of a
``Storage_Size`` of zero for an access type.  This means that no
items can be allocated from the pool, and this is recognized at compile
time, and all the overhead normally associated with maintaining a fixed
size storage pool is eliminated.  Consider the following example:

.. code-block:: ada

     procedure p is
        type R is array (Natural) of Character;
        type P is access all R;
        for P'Storage_Size use 0;
        --  Above access type intended only for interfacing purposes

        y : P;

        procedure g (m : P);
        pragma Import (C, g);

        --  ...

     begin
        --  ...
        y := new R;
     end;

As indicated in this example, these dummy storage pools are often useful in
connection with interfacing where no object will ever be allocated.  If you
compile the above example, you get the warning:

::

     p.adb:16:09: warning: allocation from empty storage pool
     p.adb:16:09: warning: Storage_Error will be raised at run time


Of course in practice, there will not be any explicit allocators in the
case of such an access declaration.

.. _Size_of_Variant_Record_Objects:

Size of Variant Record Objects
==============================

.. index:: Size, variant record objects

.. index:: Variant record objects, size

In the case of variant record objects, there is a question whether Size gives
information about a particular variant, or the maximum size required
for any variant.  Consider the following program

.. code-block:: ada

  with Text_IO; use Text_IO;
  procedure q is
     type R1 (A : Boolean := False) is record
       case A is
         when True  => X : Character;
         when False => null;
       end case;
     end record;

     V1 : R1 (False);
     V2 : R1;

  begin
     Put_Line (Integer'Image (V1'Size));
     Put_Line (Integer'Image (V2'Size));
  end q;

Here we are dealing with a variant record, where the True variant
requires 16 bits, and the False variant requires 8 bits.
In the above example, both V1 and V2 contain the False variant,
which is only 8 bits long.  However, the result of running the
program is:

::

  8
  16

The reason for the difference here is that the discriminant value of
V1 is fixed, and will always be False.  It is not possible to assign
a True variant value to V1, therefore 8 bits is sufficient.  On the
other hand, in the case of V2, the initial discriminant value is
False (from the default), but it is possible to assign a True
variant value to V2, therefore 16 bits must be allocated for V2
in the general case, even fewer bits may be needed at any particular
point during the program execution.

As can be seen from the output of this program, the ``'Size``
attribute applied to such an object in GNAT gives the actual allocated
size of the variable, which is the largest size of any of the variants.
The Ada Reference Manual is not completely clear on what choice should
be made here, but the GNAT behavior seems most consistent with the
language in the RM.

In some cases, it may be desirable to obtain the size of the current
variant, rather than the size of the largest variant.  This can be
achieved in GNAT by making use of the fact that in the case of a
subprogram parameter, GNAT does indeed return the size of the current
variant (because a subprogram has no way of knowing how much space
is actually allocated for the actual).

Consider the following modified version of the above program:

.. code-block:: ada

  with Text_IO; use Text_IO;
  procedure q is
     type R1 (A : Boolean := False) is record
       case A is
         when True  => X : Character;
         when False => null;
       end case;
     end record;

     V2 : R1;

     function Size (V : R1) return Integer is
     begin
        return V'Size;
     end Size;

  begin
     Put_Line (Integer'Image (V2'Size));
     Put_Line (Integer'Image (Size (V2)));
     V2 := (True, 'x');
     Put_Line (Integer'Image (V2'Size));
     Put_Line (Integer'Image (Size (V2)));
  end q;

The output from this program is

::

  16
  8
  16
  16

Here we see that while the ``'Size`` attribute always returns
the maximum size, regardless of the current variant value, the
``Size`` function does indeed return the size of the current
variant value.


.. _Biased_Representation:

Biased Representation
=====================

.. index:: Size for biased representation

.. index:: Biased representation

In the case of scalars with a range starting at other than zero, it is
possible in some cases to specify a size smaller than the default minimum
value, and in such cases, GNAT uses an unsigned biased representation,
in which zero is used to represent the lower bound, and successive values
represent successive values of the type.

For example, suppose we have the declaration:

.. code-block:: ada

     type Small is range -7 .. -4;
     for Small'Size use 2;

Although the default size of type ``Small`` is 4, the ``Size``
clause is accepted by GNAT and results in the following representation
scheme:

::

    -7 is represented as 2#00#
    -6 is represented as 2#01#
    -5 is represented as 2#10#
    -4 is represented as 2#11#

Biased representation is only used if the specified ``Size`` clause
cannot be accepted in any other manner.  These reduced sizes that force
biased representation can be used for all discrete types except for
enumeration types for which a representation clause is given.


.. _Value_Size_and_Object_Size_Clauses:

Value_Size and Object_Size Clauses
==================================

.. index:: Value_Size
.. index:: Object_Size
.. index:: Size, of objects

In Ada 95 and Ada 2005, ``T'Size`` for a type ``T`` is the minimum
number of bits required to hold values of type ``T``.
Although this interpretation was allowed in Ada 83, it was not required,
and this requirement in practice can cause some significant difficulties.
For example, in most Ada 83 compilers, ``Natural'Size`` was 32.
However, in Ada 95 and Ada 2005,
``Natural'Size`` is
typically 31.  This means that code may change in behavior when moving
from Ada 83 to Ada 95 or Ada 2005.  For example, consider:

.. code-block:: ada

     type Rec is record;
        A : Natural;
        B : Natural;
     end record;

     for Rec use record
        at 0  range 0 .. Natural'Size - 1;
        at 0  range Natural'Size .. 2 * Natural'Size - 1;
     end record;

In the above code, since the typical size of ``Natural`` objects
is 32 bits and ``Natural'Size`` is 31, the above code can cause
unexpected inefficient packing in Ada 95 and Ada 2005, and in general
there are cases where the fact that the object size can exceed the
size of the type causes surprises.

To help get around this problem GNAT provides two implementation
defined attributes, ``Value_Size`` and ``Object_Size``.  When
applied to a type, these attributes yield the size of the type
(corresponding to the RM defined size attribute), and the size of
objects of the type respectively.

The ``Object_Size`` is used for determining the default size of
objects and components.  This size value can be referred to using the
``Object_Size`` attribute.  The phrase 'is used' here means that it is
the basis of the determination of the size.  The backend is free to
pad this up if necessary for efficiency, e.g., an 8-bit stand-alone
character might be stored in 32 bits on a machine with no efficient
byte access instructions such as the Alpha.

The default rules for the value of ``Object_Size`` for
discrete types are as follows:

*
  The ``Object_Size`` for base subtypes reflect the natural hardware
  size in bits (run the compiler with *-gnatS* to find those values
  for numeric types). Enumeration types and fixed-point base subtypes have
  8, 16, 32, or 64 bits for this size, depending on the range of values
  to be stored.

*
  The ``Object_Size`` of a subtype is the same as the
  ``Object_Size`` of
  the type from which it is obtained.

*
  The ``Object_Size`` of a derived base type is copied from the parent
  base type, and the ``Object_Size`` of a derived first subtype is copied
  from the parent first subtype.

The ``Value_Size`` attribute
is the (minimum) number of bits required to store a value
of the type.
This value is used to determine how tightly to pack
records or arrays with components of this type, and also affects
the semantics of unchecked conversion (unchecked conversions where
the ``Value_Size`` values differ generate a warning, and are potentially
target dependent).

The default rules for the value of ``Value_Size`` are as follows:

*
  The ``Value_Size`` for a base subtype is the minimum number of bits
  required to store all values of the type (including the sign bit
  only if negative values are possible).

*
  If a subtype statically matches the first subtype of a given type, then it has
  by default the same ``Value_Size`` as the first subtype.  This is a
  consequence of RM 13.1(14): "if two subtypes statically match,
  then their subtype-specific aspects are the same".)

*
  All other subtypes have a ``Value_Size`` corresponding to the minimum
  number of bits required to store all values of the subtype.  For
  dynamic bounds, it is assumed that the value can range down or up
  to the corresponding bound of the ancestor

The RM defined attribute ``Size`` corresponds to the
``Value_Size`` attribute.

The ``Size`` attribute may be defined for a first-named subtype.  This sets
the ``Value_Size`` of
the first-named subtype to the given value, and the
``Object_Size`` of this first-named subtype to the given value padded up
to an appropriate boundary.  It is a consequence of the default rules
above that this ``Object_Size`` will apply to all further subtypes.  On the
other hand, ``Value_Size`` is affected only for the first subtype, any
dynamic subtypes obtained from it directly, and any statically matching
subtypes.  The ``Value_Size`` of any other static subtypes is not affected.

``Value_Size`` and
``Object_Size`` may be explicitly set for any subtype using
an attribute definition clause.  Note that the use of these attributes
can cause the RM 13.1(14) rule to be violated.  If two access types
reference aliased objects whose subtypes have differing ``Object_Size``
values as a result of explicit attribute definition clauses, then it
is illegal to convert from one access subtype to the other. For a more
complete description of this additional legality rule, see the
description of the ``Object_Size`` attribute.

To get a feel for the difference, consider the following examples (note
that in each case the base is ``Short_Short_Integer`` with a size of 8):

+---------------------------------------------+-------------+-------------+
|Type or subtype declaration                  | Object_Size |   Value_Size|
+=============================================+=============+=============+
|``type x1 is range 0 .. 5;``                 |  8          |    3        |
+---------------------------------------------+-------------+-------------+
|``type x2 is range 0 .. 5;``                 | 16          |   12        |
|``for x2'size use 12;``                      |             |             |
+---------------------------------------------+-------------+-------------+
|``subtype x3 is x2 range 0 .. 3;``           | 16          |    2        |
+---------------------------------------------+-------------+-------------+
|``subtype x4 is x2'base range 0 .. 10;``     |  8          |    4        |
+---------------------------------------------+-------------+-------------+
|``dynamic : x2'Base range -64 .. +63;``      |             |             |
+---------------------------------------------+-------------+-------------+
|``subtype x5 is x2 range 0 .. dynamic;``     | 16          |    3*       |
+---------------------------------------------+-------------+-------------+
|``subtype x6 is x2'base range 0 .. dynamic;``|  8          |    7*       |
+---------------------------------------------+-------------+-------------+

Note: the entries marked '*' are not actually specified by the Ada
Reference Manual, which has nothing to say about size in the dynamic
case. What GNAT does is to allocate sufficient bits to accomodate any
possible dynamic values for the bounds at run-time.

So far, so good, but GNAT has to obey the RM rules, so the question is
under what conditions must the RM ``Size`` be used.
The following is a list
of the occasions on which the RM ``Size`` must be used:

*
  Component size for packed arrays or records

*
  Value of the attribute ``Size`` for a type

*
  Warning about sizes not matching for unchecked conversion

For record types, the ``Object_Size`` is always a multiple of the
alignment of the type (this is true for all types). In some cases the
``Value_Size`` can be smaller. Consider:


.. code-block:: ada

     type R is record
       X : Integer;
       Y : Character;
     end record;


On a typical 32-bit architecture, the X component will be four bytes, and
require four-byte alignment, and the Y component will be one byte. In this
case ``R'Value_Size`` will be 40 (bits) since this is the minimum size
required to store a value of this type, and for example, it is permissible
to have a component of type R in an outer array whose component size is
specified to be 48 bits. However, ``R'Object_Size`` will be 64 (bits),
since it must be rounded up so that this value is a multiple of the
alignment (4 bytes = 32 bits).

For all other types, the ``Object_Size``
and ``Value_Size`` are the same (and equivalent to the RM attribute ``Size``).
Only ``Size`` may be specified for such types.

Note that ``Value_Size`` can be used to force biased representation
for a particular subtype. Consider this example:


.. code-block:: ada

     type R is (A, B, C, D, E, F);
     subtype RAB is R range A .. B;
     subtype REF is R range E .. F;


By default, ``RAB``
has a size of 1 (sufficient to accommodate the representation
of ``A`` and ``B``, 0 and 1), and ``REF``
has a size of 3 (sufficient to accommodate the representation
of ``E`` and ``F``, 4 and 5). But if we add the
following ``Value_Size`` attribute definition clause:


.. code-block:: ada

     for REF'Value_Size use 1;


then biased representation is forced for ``REF``,
and 0 will represent ``E`` and 1 will represent ``F``.
A warning is issued when a ``Value_Size`` attribute
definition clause forces biased representation. This
warning can be turned off using :switch:`-gnatw.B`.

.. _Component_Size_Clauses:

Component_Size Clauses
======================

.. index:: Component_Size Clause

Normally, the value specified in a component size clause must be consistent
with the subtype of the array component with regard to size and alignment.
In other words, the value specified must be at least equal to the size
of this subtype, and must be a multiple of the alignment value.

In addition, component size clauses are allowed which cause the array
to be packed, by specifying a smaller value.  A first case is for
component size values in the range 1 through 63.  The value specified
must not be smaller than the Size of the subtype.  GNAT will accurately
honor all packing requests in this range.  For example, if we have:


.. code-block:: ada

  type r is array (1 .. 8) of Natural;
  for r'Component_Size use 31;


then the resulting array has a length of 31 bytes (248 bits = 8 * 31).
Of course access to the components of such an array is considerably
less efficient than if the natural component size of 32 is used.
A second case is when the subtype of the component is a record type
padded because of its default alignment.  For example, if we have:


.. code-block:: ada

  type r is record
    i : Integer;
    j : Integer;
    b : Boolean;
  end record;

  type a is array (1 .. 8) of r;
  for a'Component_Size use 72;


then the resulting array has a length of 72 bytes, instead of 96 bytes
if the alignment of the record (4) was obeyed.

Note that there is no point in giving both a component size clause
and a pragma Pack for the same array type. if such duplicate
clauses are given, the pragma Pack will be ignored.

.. _Bit_Order_Clauses:

Bit_Order Clauses
=================

.. index:: Bit_Order Clause

.. index:: bit ordering

.. index:: ordering, of bits

For record subtypes, GNAT permits the specification of the ``Bit_Order``
attribute.  The specification may either correspond to the default bit
order for the target, in which case the specification has no effect and
places no additional restrictions, or it may be for the non-standard
setting (that is the opposite of the default).

In the case where the non-standard value is specified, the effect is
to renumber bits within each byte, but the ordering of bytes is not
affected.  There are certain
restrictions placed on component clauses as follows:


* Components fitting within a single storage unit.

  These are unrestricted, and the effect is merely to renumber bits.  For
  example if we are on a little-endian machine with ``Low_Order_First``
  being the default, then the following two declarations have exactly
  the same effect:


  ::

       type R1 is record
          A : Boolean;
          B : Integer range 1 .. 120;
       end record;

       for R1 use record
          A at 0 range 0 .. 0;
          B at 0 range 1 .. 7;
       end record;

       type R2 is record
          A : Boolean;
          B : Integer range 1 .. 120;
       end record;

       for R2'Bit_Order use High_Order_First;

       for R2 use record
          A at 0 range 7 .. 7;
          B at 0 range 0 .. 6;
       end record;


  The useful application here is to write the second declaration with the
  ``Bit_Order`` attribute definition clause, and know that it will be treated
  the same, regardless of whether the target is little-endian or big-endian.

* Components occupying an integral number of bytes.

  These are components that exactly fit in two or more bytes.  Such component
  declarations are allowed, but have no effect, since it is important to realize
  that the ``Bit_Order`` specification does not affect the ordering of bytes.
  In particular, the following attempt at getting an endian-independent integer
  does not work:


  ::

       type R2 is record
          A : Integer;
       end record;

       for R2'Bit_Order use High_Order_First;

       for R2 use record
          A at 0 range 0 .. 31;
       end record;


  This declaration will result in a little-endian integer on a
  little-endian machine, and a big-endian integer on a big-endian machine.
  If byte flipping is required for interoperability between big- and
  little-endian machines, this must be explicitly programmed.  This capability
  is not provided by ``Bit_Order``.

* Components that are positioned across byte boundaries.

  but do not occupy an integral number of bytes.  Given that bytes are not
  reordered, such fields would occupy a non-contiguous sequence of bits
  in memory, requiring non-trivial code to reassemble.  They are for this
  reason not permitted, and any component clause specifying such a layout
  will be flagged as illegal by GNAT.


Since the misconception that Bit_Order automatically deals with all
endian-related incompatibilities is a common one, the specification of
a component field that is an integral number of bytes will always
generate a warning.  This warning may be suppressed using ``pragma Warnings (Off)``
if desired.  The following section contains additional
details regarding the issue of byte ordering.

.. _Effect_of_Bit_Order_on_Byte_Ordering:

Effect of Bit_Order on Byte Ordering
====================================

.. index:: byte ordering

.. index:: ordering, of bytes

In this section we will review the effect of the ``Bit_Order`` attribute
definition clause on byte ordering.  Briefly, it has no effect at all, but
a detailed example will be helpful.  Before giving this
example, let us review the precise
definition of the effect of defining ``Bit_Order``.  The effect of a
non-standard bit order is described in section 13.5.3 of the Ada
Reference Manual:

   "2   A bit ordering is a method of interpreting the meaning of
   the storage place attributes."

To understand the precise definition of storage place attributes in
this context, we visit section 13.5.1 of the manual:

   "13   A record_representation_clause (without the mod_clause)
   specifies the layout.  The storage place attributes (see 13.5.2)
   are taken from the values of the position, first_bit, and last_bit
   expressions after normalizing those values so that first_bit is
   less than Storage_Unit."

The critical point here is that storage places are taken from
the values after normalization, not before.  So the ``Bit_Order``
interpretation applies to normalized values.  The interpretation
is described in the later part of the 13.5.3 paragraph:

   "2   A bit ordering is a method of interpreting the meaning of
   the storage place attributes.  High_Order_First (known in the
   vernacular as 'big endian') means that the first bit of a
   storage element (bit 0) is the most significant bit (interpreting
   the sequence of bits that represent a component as an unsigned
   integer value).  Low_Order_First (known in the vernacular as
   'little endian') means the opposite: the first bit is the
   least significant."

Note that the numbering is with respect to the bits of a storage
unit.  In other words, the specification affects only the numbering
of bits within a single storage unit.

We can make the effect clearer by giving an example.

Suppose that we have an external device which presents two bytes, the first
byte presented, which is the first (low addressed byte) of the two byte
record is called Master, and the second byte is called Slave.

The left most (most significant bit is called Control for each byte, and
the remaining 7 bits are called V1, V2, ... V7, where V7 is the rightmost
(least significant) bit.

On a big-endian machine, we can write the following representation clause


.. code-block:: ada

     type Data is record
        Master_Control : Bit;
        Master_V1      : Bit;
        Master_V2      : Bit;
        Master_V3      : Bit;
        Master_V4      : Bit;
        Master_V5      : Bit;
        Master_V6      : Bit;
        Master_V7      : Bit;
        Slave_Control  : Bit;
        Slave_V1       : Bit;
        Slave_V2       : Bit;
        Slave_V3       : Bit;
        Slave_V4       : Bit;
        Slave_V5       : Bit;
        Slave_V6       : Bit;
        Slave_V7       : Bit;
     end record;

     for Data use record
        Master_Control at 0 range 0 .. 0;
        Master_V1      at 0 range 1 .. 1;
        Master_V2      at 0 range 2 .. 2;
        Master_V3      at 0 range 3 .. 3;
        Master_V4      at 0 range 4 .. 4;
        Master_V5      at 0 range 5 .. 5;
        Master_V6      at 0 range 6 .. 6;
        Master_V7      at 0 range 7 .. 7;
        Slave_Control  at 1 range 0 .. 0;
        Slave_V1       at 1 range 1 .. 1;
        Slave_V2       at 1 range 2 .. 2;
        Slave_V3       at 1 range 3 .. 3;
        Slave_V4       at 1 range 4 .. 4;
        Slave_V5       at 1 range 5 .. 5;
        Slave_V6       at 1 range 6 .. 6;
        Slave_V7       at 1 range 7 .. 7;
     end record;


Now if we move this to a little endian machine, then the bit ordering within
the byte is backwards, so we have to rewrite the record rep clause as:


.. code-block:: ada

     for Data use record
        Master_Control at 0 range 7 .. 7;
        Master_V1      at 0 range 6 .. 6;
        Master_V2      at 0 range 5 .. 5;
        Master_V3      at 0 range 4 .. 4;
        Master_V4      at 0 range 3 .. 3;
        Master_V5      at 0 range 2 .. 2;
        Master_V6      at 0 range 1 .. 1;
        Master_V7      at 0 range 0 .. 0;
        Slave_Control  at 1 range 7 .. 7;
        Slave_V1       at 1 range 6 .. 6;
        Slave_V2       at 1 range 5 .. 5;
        Slave_V3       at 1 range 4 .. 4;
        Slave_V4       at 1 range 3 .. 3;
        Slave_V5       at 1 range 2 .. 2;
        Slave_V6       at 1 range 1 .. 1;
        Slave_V7       at 1 range 0 .. 0;
     end record;


It is a nuisance to have to rewrite the clause, especially if
the code has to be maintained on both machines.  However,
this is a case that we can handle with the
``Bit_Order`` attribute if it is implemented.
Note that the implementation is not required on byte addressed
machines, but it is indeed implemented in GNAT.
This means that we can simply use the
first record clause, together with the declaration


.. code-block:: ada

     for Data'Bit_Order use High_Order_First;


and the effect is what is desired, namely the layout is exactly the same,
independent of whether the code is compiled on a big-endian or little-endian
machine.

The important point to understand is that byte ordering is not affected.
A ``Bit_Order`` attribute definition never affects which byte a field
ends up in, only where it ends up in that byte.
To make this clear, let us rewrite the record rep clause of the previous
example as:


.. code-block:: ada

     for Data'Bit_Order use High_Order_First;
     for Data use record
        Master_Control at 0 range  0 .. 0;
        Master_V1      at 0 range  1 .. 1;
        Master_V2      at 0 range  2 .. 2;
        Master_V3      at 0 range  3 .. 3;
        Master_V4      at 0 range  4 .. 4;
        Master_V5      at 0 range  5 .. 5;
        Master_V6      at 0 range  6 .. 6;
        Master_V7      at 0 range  7 .. 7;
        Slave_Control  at 0 range  8 .. 8;
        Slave_V1       at 0 range  9 .. 9;
        Slave_V2       at 0 range 10 .. 10;
        Slave_V3       at 0 range 11 .. 11;
        Slave_V4       at 0 range 12 .. 12;
        Slave_V5       at 0 range 13 .. 13;
        Slave_V6       at 0 range 14 .. 14;
        Slave_V7       at 0 range 15 .. 15;
     end record;


This is exactly equivalent to saying (a repeat of the first example):


.. code-block:: ada

     for Data'Bit_Order use High_Order_First;
     for Data use record
        Master_Control at 0 range 0 .. 0;
        Master_V1      at 0 range 1 .. 1;
        Master_V2      at 0 range 2 .. 2;
        Master_V3      at 0 range 3 .. 3;
        Master_V4      at 0 range 4 .. 4;
        Master_V5      at 0 range 5 .. 5;
        Master_V6      at 0 range 6 .. 6;
        Master_V7      at 0 range 7 .. 7;
        Slave_Control  at 1 range 0 .. 0;
        Slave_V1       at 1 range 1 .. 1;
        Slave_V2       at 1 range 2 .. 2;
        Slave_V3       at 1 range 3 .. 3;
        Slave_V4       at 1 range 4 .. 4;
        Slave_V5       at 1 range 5 .. 5;
        Slave_V6       at 1 range 6 .. 6;
        Slave_V7       at 1 range 7 .. 7;
     end record;


Why are they equivalent? Well take a specific field, the ``Slave_V2``
field.  The storage place attributes are obtained by normalizing the
values given so that the ``First_Bit`` value is less than 8.  After
normalizing the values (0,10,10) we get (1,2,2) which is exactly what
we specified in the other case.

Now one might expect that the ``Bit_Order`` attribute might affect
bit numbering within the entire record component (two bytes in this
case, thus affecting which byte fields end up in), but that is not
the way this feature is defined, it only affects numbering of bits,
not which byte they end up in.

Consequently it never makes sense to specify a starting bit number
greater than 7 (for a byte addressable field) if an attribute
definition for ``Bit_Order`` has been given, and indeed it
may be actively confusing to specify such a value, so the compiler
generates a warning for such usage.

If you do need to control byte ordering then appropriate conditional
values must be used.  If in our example, the slave byte came first on
some machines we might write:

.. code-block:: ada

     Master_Byte_First constant Boolean := ...;

     Master_Byte : constant Natural :=
                     1 - Boolean'Pos (Master_Byte_First);
     Slave_Byte  : constant Natural :=
                     Boolean'Pos (Master_Byte_First);

     for Data'Bit_Order use High_Order_First;
     for Data use record
        Master_Control at Master_Byte range 0 .. 0;
        Master_V1      at Master_Byte range 1 .. 1;
        Master_V2      at Master_Byte range 2 .. 2;
        Master_V3      at Master_Byte range 3 .. 3;
        Master_V4      at Master_Byte range 4 .. 4;
        Master_V5      at Master_Byte range 5 .. 5;
        Master_V6      at Master_Byte range 6 .. 6;
        Master_V7      at Master_Byte range 7 .. 7;
        Slave_Control  at Slave_Byte  range 0 .. 0;
        Slave_V1       at Slave_Byte  range 1 .. 1;
        Slave_V2       at Slave_Byte  range 2 .. 2;
        Slave_V3       at Slave_Byte  range 3 .. 3;
        Slave_V4       at Slave_Byte  range 4 .. 4;
        Slave_V5       at Slave_Byte  range 5 .. 5;
        Slave_V6       at Slave_Byte  range 6 .. 6;
        Slave_V7       at Slave_Byte  range 7 .. 7;
     end record;

Now to switch between machines, all that is necessary is
to set the boolean constant ``Master_Byte_First`` in
an appropriate manner.

.. _Pragma_Pack_for_Arrays:

Pragma Pack for Arrays
======================

.. index:: Pragma Pack (for arrays)

Pragma ``Pack`` applied to an array has an effect that depends upon whether the
component type is *packable*.  For a component type to be *packable*, it must
be one of the following cases:

* Any elementary type.

* Any small packed array type with a static size.

* Any small simple record type with a static size.

For all these cases, if the component subtype size is in the range
1 through 64, then the effect of the pragma ``Pack`` is exactly as though a
component size were specified giving the component subtype size.

All other types are non-packable, they occupy an integral number of storage
units and the only effect of pragma Pack is to remove alignment gaps.

For example if we have:

.. code-block:: ada

     type r is range 0 .. 17;

     type ar is array (1 .. 8) of r;
     pragma Pack (ar);

Then the component size of ``ar`` will be set to 5 (i.e., to ``r'size``,
and the size of the array ``ar`` will be exactly 40 bits).

Note that in some cases this rather fierce approach to packing can produce
unexpected effects.  For example, in Ada 95 and Ada 2005,
subtype ``Natural`` typically has a size of 31, meaning that if you
pack an array of ``Natural``, you get 31-bit
close packing, which saves a few bits, but results in far less efficient
access.  Since many other Ada compilers will ignore such a packing request,
GNAT will generate a warning on some uses of pragma ``Pack`` that it guesses
might not be what is intended.  You can easily remove this warning by
using an explicit ``Component_Size`` setting instead, which never generates
a warning, since the intention of the programmer is clear in this case.

GNAT treats packed arrays in one of two ways.  If the size of the array is
known at compile time and is less than 64 bits, then internally the array
is represented as a single modular type, of exactly the appropriate number
of bits.  If the length is greater than 63 bits, or is not known at compile
time, then the packed array is represented as an array of bytes, and the
length is always a multiple of 8 bits.

Note that to represent a packed array as a modular type, the alignment must
be suitable for the modular type involved. For example, on typical machines
a 32-bit packed array will be represented by a 32-bit modular integer with
an alignment of four bytes. If you explicitly override the default alignment
with an alignment clause that is too small, the modular representation
cannot be used. For example, consider the following set of declarations:

.. code-block:: ada

     type R is range 1 .. 3;
     type S is array (1 .. 31) of R;
     for S'Component_Size use 2;
     for S'Size use 62;
     for S'Alignment use 1;

If the alignment clause were not present, then a 62-bit modular
representation would be chosen (typically with an alignment of 4 or 8
bytes depending on the target). But the default alignment is overridden
with the explicit alignment clause. This means that the modular
representation cannot be used, and instead the array of bytes
representation must be used, meaning that the length must be a multiple
of 8. Thus the above set of declarations will result in a diagnostic
rejecting the size clause and noting that the minimum size allowed is 64.

.. index:: Pragma Pack (for type Natural)

.. index:: Pragma Pack warning

One special case that is worth noting occurs when the base type of the
component size is 8/16/32 and the subtype is one bit less. Notably this
occurs with subtype ``Natural``. Consider:

.. code-block:: ada

     type Arr is array (1 .. 32) of Natural;
     pragma Pack (Arr);

In all commonly used Ada 83 compilers, this pragma Pack would be ignored,
since typically ``Natural'Size`` is 32 in Ada 83, and in any case most
Ada 83 compilers did not attempt 31 bit packing.

In Ada 95 and Ada 2005, ``Natural'Size`` is required to be 31. Furthermore,
GNAT really does pack 31-bit subtype to 31 bits. This may result in a
substantial unintended performance penalty when porting legacy Ada 83 code.
To help prevent this, GNAT generates a warning in such cases. If you really
want 31 bit packing in a case like this, you can set the component size
explicitly:

.. code-block:: ada

     type Arr is array (1 .. 32) of Natural;
     for Arr'Component_Size use 31;

Here 31-bit packing is achieved as required, and no warning is generated,
since in this case the programmer intention is clear.

.. _Pragma_Pack_for_Records:

Pragma Pack for Records
=======================

.. index:: Pragma Pack (for records)

Pragma ``Pack`` applied to a record will pack the components to reduce
wasted space from alignment gaps and by reducing the amount of space
taken by components.  We distinguish between *packable* components and
*non-packable* components.
Components of the following types are considered packable:

* Components of an elementary type are packable unless they are aliased,
  independent, or of an atomic type.

* Small packed arrays, where the size is statically known, are represented
  internally as modular integers, and so they are also packable.

* Small simple records, where the size is statically known, are also packable.

For all these cases, if the ``'Size`` value is in the range 1 through 64, the
components occupy the exact number of bits corresponding to this value
and are packed with no padding bits, i.e. they can start on an arbitrary
bit boundary.

All other types are non-packable, they occupy an integral number of storage
units and the only effect of pragma ``Pack`` is to remove alignment gaps.

For example, consider the record

.. code-block:: ada

     type Rb1 is array (1 .. 13) of Boolean;
     pragma Pack (Rb1);

     type Rb2 is array (1 .. 65) of Boolean;
     pragma Pack (Rb2);

     type AF is new Float with Atomic;

     type X2 is record
        L1 : Boolean;
        L2 : Duration;
        L3 : AF;
        L4 : Boolean;
        L5 : Rb1;
        L6 : Rb2;
     end record;
     pragma Pack (X2);

The representation for the record ``X2`` is as follows:

.. code-block:: ada

  for X2'Size use 224;
  for X2 use record
     L1 at  0 range  0 .. 0;
     L2 at  0 range  1 .. 64;
     L3 at 12 range  0 .. 31;
     L4 at 16 range  0 .. 0;
     L5 at 16 range  1 .. 13;
     L6 at 18 range  0 .. 71;
  end record;

Studying this example, we see that the packable fields ``L1``
and ``L2`` are
of length equal to their sizes, and placed at specific bit boundaries (and
not byte boundaries) to
eliminate padding.  But ``L3`` is of a non-packable float type (because
it is aliased), so it is on the next appropriate alignment boundary.

The next two fields are fully packable, so ``L4`` and ``L5`` are
minimally packed with no gaps.  However, type ``Rb2`` is a packed
array that is longer than 64 bits, so it is itself non-packable.  Thus
the ``L6`` field is aligned to the next byte boundary, and takes an
integral number of bytes, i.e., 72 bits.

.. _Record_Representation_Clauses:

Record Representation Clauses
=============================

.. index:: Record Representation Clause

Record representation clauses may be given for all record types, including
types obtained by record extension.  Component clauses are allowed for any
static component.  The restrictions on component clauses depend on the type
of the component.

.. index:: Component Clause

For all components of an elementary type, the only restriction on component
clauses is that the size must be at least the ``'Size`` value of the type
(actually the Value_Size).  There are no restrictions due to alignment,
and such components may freely cross storage boundaries.

Packed arrays with a size up to and including 64 bits are represented
internally using a modular type with the appropriate number of bits, and
thus the same lack of restriction applies.  For example, if you declare:

.. code-block:: ada

     type R is array (1 .. 49) of Boolean;
     pragma Pack (R);
     for R'Size use 49;

then a component clause for a component of type ``R`` may start on any
specified bit boundary, and may specify a value of 49 bits or greater.

For packed bit arrays that are longer than 64 bits, there are two
cases. If the component size is a power of 2 (1,2,4,8,16,32 bits),
including the important case of single bits or boolean values, then
there are no limitations on placement of such components, and they
may start and end at arbitrary bit boundaries.

If the component size is not a power of 2 (e.g., 3 or 5), then
an array of this type longer than 64 bits must always be placed on
on a storage unit (byte) boundary and occupy an integral number
of storage units (bytes). Any component clause that does not
meet this requirement will be rejected.

Any aliased component, or component of an aliased type, must
have its normal alignment and size. A component clause that
does not meet this requirement will be rejected.

The tag field of a tagged type always occupies an address sized field at
the start of the record.  No component clause may attempt to overlay this
tag. When a tagged type appears as a component, the tag field must have
proper alignment

In the case of a record extension ``T1``, of a type ``T``, no component clause applied
to the type ``T1`` can specify a storage location that would overlap the first
``T'Size`` bytes of the record.

For all other component types, including non-bit-packed arrays,
the component can be placed at an arbitrary bit boundary,
so for example, the following is permitted:

.. code-block:: ada

     type R is array (1 .. 10) of Boolean;
     for R'Size use 80;

     type Q is record
        G, H : Boolean;
        L, M : R;
     end record;

     for Q use record
        G at 0 range  0 ..   0;
        H at 0 range  1 ..   1;
        L at 0 range  2 ..  81;
        R at 0 range 82 .. 161;
     end record;

.. _Handling_of_Records_with_Holes:

Handling of Records with Holes
==============================

.. index:: Handling of Records with Holes

As a result of alignment considerations, records may contain "holes"
or gaps
which do not correspond to the data bits of any of the components.
Record representation clauses can also result in holes in records.

GNAT does not attempt to clear these holes, so in record objects,
they should be considered to hold undefined rubbish. The generated
equality routine just tests components so does not access these
undefined bits, and assignment and copy operations may or may not
preserve the contents of these holes (for assignments, the holes
in the target will in practice contain either the bits that are
present in the holes in the source, or the bits that were present
in the target before the assignment).

If it is necessary to ensure that holes in records have all zero
bits, then record objects for which this initialization is desired
should be explicitly set to all zero values using Unchecked_Conversion
or address overlays. For example

.. code-block:: ada

  type HRec is record
     C : Character;
     I : Integer;
  end record;

On typical machines, integers need to be aligned on a four-byte
boundary, resulting in three bytes of undefined rubbish following
the 8-bit field for C. To ensure that the hole in a variable of
type HRec is set to all zero bits,
you could for example do:

.. code-block:: ada

  type Base is record
     Dummy1, Dummy2 : Integer := 0;
  end record;

  BaseVar : Base;
  RealVar : Hrec;
  for RealVar'Address use BaseVar'Address;


Now the 8-bytes of the value of RealVar start out containing all zero
bits. A safer approach is to just define dummy fields, avoiding the
holes, as in:

.. code-block:: ada

  type HRec is record
     C      : Character;
     Dummy1 : Short_Short_Integer := 0;
     Dummy2 : Short_Short_Integer := 0;
     Dummy3 : Short_Short_Integer := 0;
     I      : Integer;
  end record;

And to make absolutely sure that the intent of this is followed, you
can use representation clauses:

.. code-block:: ada

  for Hrec use record
     C      at 0 range 0 .. 7;
     Dummy1 at 1 range 0 .. 7;
     Dummy2 at 2 range 0 .. 7;
     Dummy3 at 3 range 0 .. 7;
     I      at 4 range 0 .. 31;
  end record;
  for Hrec'Size use 64;


.. _Enumeration_Clauses:

Enumeration Clauses
===================

The only restriction on enumeration clauses is that the range of values
must be representable.  For the signed case, if one or more of the
representation values are negative, all values must be in the range:

.. code-block:: ada

     System.Min_Int .. System.Max_Int

For the unsigned case, where all values are nonnegative, the values must
be in the range:

.. code-block:: ada

     0 .. System.Max_Binary_Modulus;


A *confirming* representation clause is one in which the values range
from 0 in sequence, i.e., a clause that confirms the default representation
for an enumeration type.
Such a confirming representation
is permitted by these rules, and is specially recognized by the compiler so
that no extra overhead results from the use of such a clause.

If an array has an index type which is an enumeration type to which an
enumeration clause has been applied, then the array is stored in a compact
manner.  Consider the declarations:

.. code-block:: ada

     type r is (A, B, C);
     for r use (A => 1, B => 5, C => 10);
     type t is array (r) of Character;

The array type t corresponds to a vector with exactly three elements and
has a default size equal to ``3*Character'Size``.  This ensures efficient
use of space, but means that accesses to elements of the array will incur
the overhead of converting representation values to the corresponding
positional values, (i.e., the value delivered by the ``Pos`` attribute).


.. _Address_Clauses:

Address Clauses
===============
.. index:: Address Clause

The reference manual allows a general restriction on representation clauses,
as found in RM 13.1(22):

   "An implementation need not support representation
   items containing nonstatic expressions, except that
   an implementation should support a representation item
   for a given entity if each nonstatic expression in the
   representation item is a name that statically denotes
   a constant declared before the entity."

In practice this is applicable only to address clauses, since this is the
only case in which a nonstatic expression is permitted by the syntax.  As
the AARM notes in sections 13.1 (22.a-22.h):

   22.a   Reason: This is to avoid the following sort of thing:

   22.b        X : Integer := F(...);
   Y : Address := G(...);
   for X'Address use Y;

   22.c   In the above, we have to evaluate the
   initialization expression for X before we
   know where to put the result.  This seems
   like an unreasonable implementation burden.

   22.d   The above code should instead be written
   like this:

   22.e        Y : constant Address := G(...);
   X : Integer := F(...);
   for X'Address use Y;

   22.f   This allows the expression 'Y' to be safely
   evaluated before X is created.

   22.g   The constant could be a formal parameter of mode in.

   22.h   An implementation can support other nonstatic
   expressions if it wants to.  Expressions of type
   Address are hardly ever static, but their value
   might be known at compile time anyway in many
   cases.

GNAT does indeed permit many additional cases of nonstatic expressions.  In
particular, if the type involved is elementary there are no restrictions
(since in this case, holding a temporary copy of the initialization value,
if one is present, is inexpensive).  In addition, if there is no implicit or
explicit initialization, then there are no restrictions.  GNAT will reject
only the case where all three of these conditions hold:

*
  The type of the item is non-elementary (e.g., a record or array).

*
  There is explicit or implicit initialization required for the object.
  Note that access values are always implicitly initialized.

*
  The address value is nonstatic.  Here GNAT is more permissive than the
  RM, and allows the address value to be the address of a previously declared
  stand-alone variable, as long as it does not itself have an address clause.

  ::

               Anchor  : Some_Initialized_Type;
               Overlay : Some_Initialized_Type;
               for Overlay'Address use Anchor'Address;

  However, the prefix of the address clause cannot be an array component, or
  a component of a discriminated record.

As noted above in section 22.h, address values are typically nonstatic.  In
particular the To_Address function, even if applied to a literal value, is
a nonstatic function call.  To avoid this minor annoyance, GNAT provides
the implementation defined attribute 'To_Address.  The following two
expressions have identical values:

.. index:: Attribute
.. index:: To_Address

.. code-block:: ada

     To_Address (16#1234_0000#)
     System'To_Address (16#1234_0000#);

except that the second form is considered to be a static expression, and
thus when used as an address clause value is always permitted.

Additionally, GNAT treats as static an address clause that is an
unchecked_conversion of a static integer value.  This simplifies the porting
of legacy code, and provides a portable equivalent to the GNAT attribute
``To_Address``.

Another issue with address clauses is the interaction with alignment
requirements.  When an address clause is given for an object, the address
value must be consistent with the alignment of the object (which is usually
the same as the alignment of the type of the object).  If an address clause
is given that specifies an inappropriately aligned address value, then the
program execution is erroneous.

Since this source of erroneous behavior can have unfortunate effects on
machines with strict alignment requirements, GNAT
checks (at compile time if possible, generating a warning, or at execution
time with a run-time check) that the alignment is appropriate.  If the
run-time check fails, then ``Program_Error`` is raised.  This run-time
check is suppressed if range checks are suppressed, or if the special GNAT
check Alignment_Check is suppressed, or if
``pragma Restrictions (No_Elaboration_Code)`` is in effect. It is also
suppressed by default on non-strict alignment machines (such as the x86).

Finally, GNAT does not permit overlaying of objects of class-wide types. In
most cases, the compiler can detect an attempt at such overlays and will
generate a warning at compile time and a Program_Error exception at run time.

.. index:: Export

An address clause cannot be given for an exported object.  More
understandably the real restriction is that objects with an address
clause cannot be exported.  This is because such variables are not
defined by the Ada program, so there is no external object to export.

.. index:: Import

It is permissible to give an address clause and a pragma Import for the
same object.  In this case, the variable is not really defined by the
Ada program, so there is no external symbol to be linked.  The link name
and the external name are ignored in this case.  The reason that we allow this
combination is that it provides a useful idiom to avoid unwanted
initializations on objects with address clauses.

When an address clause is given for an object that has implicit or
explicit initialization, then by default initialization takes place.  This
means that the effect of the object declaration is to overwrite the
memory at the specified address.  This is almost always not what the
programmer wants, so GNAT will output a warning:

::

    with System;
    package G is
       type R is record
          M : Integer := 0;
       end record;

       Ext : R;
       for Ext'Address use System'To_Address (16#1234_1234#);
           |
    >>> warning: implicit initialization of "Ext" may
        modify overlaid storage
    >>> warning: use pragma Import for "Ext" to suppress
        initialization (RM B(24))

    end G;

As indicated by the warning message, the solution is to use a (dummy) pragma
Import to suppress this initialization.  The pragma tell the compiler that the
object is declared and initialized elsewhere.  The following package compiles
without warnings (and the initialization is suppressed):

.. code-block:: ada

     with System;
     package G is
        type R is record
           M : Integer := 0;
        end record;

        Ext : R;
        for Ext'Address use System'To_Address (16#1234_1234#);
        pragma Import (Ada, Ext);
     end G;


A final issue with address clauses involves their use for overlaying
variables, as in the following example:

.. index:: Overlaying of objects

.. code-block:: ada

    A : Integer;
    B : Integer;
    for B'Address use A'Address;


or alternatively, using the form recommended by the RM:

.. code-block:: ada

    A    : Integer;
    Addr : constant Address := A'Address;
    B    : Integer;
    for B'Address use Addr;


In both of these cases, ``A`` and ``B`` become aliased to one another
via the address clause. This use of address clauses to overlay
variables, achieving an effect similar to unchecked conversion
was erroneous in Ada 83, but in Ada 95 and Ada 2005
the effect is implementation defined. Furthermore, the
Ada RM specifically recommends that in a situation
like this, ``B`` should be subject to the following
implementation advice (RM 13.3(19)):

   "19  If the Address of an object is specified, or it is imported
   or exported, then the implementation should not perform
   optimizations based on assumptions of no aliases."

GNAT follows this recommendation, and goes further by also applying
this recommendation to the overlaid variable (``A`` in the above example)
in this case. This means that the overlay works "as expected", in that
a modification to one of the variables will affect the value of the other.

More generally, GNAT interprets this recommendation conservatively for
address clauses: in the cases other than overlays, it considers that the
object is effectively subject to pragma ``Volatile`` and implements the
associated semantics.

Note that when address clause overlays are used in this way, there is an
issue of unintentional initialization, as shown by this example:

::

  package Overwrite_Record is
     type R is record
        A : Character := 'C';
        B : Character := 'A';
     end record;
     X : Short_Integer := 3;
     Y : R;
     for Y'Address use X'Address;
         |
  >>> warning: default initialization of "Y" may
      modify "X", use pragma Import for "Y" to
      suppress initialization (RM B.1(24))

  end Overwrite_Record;

Here the default initialization of ``Y`` will clobber the value
of ``X``, which justifies the warning. The warning notes that
this effect can be eliminated by adding a ``pragma Import``
which suppresses the initialization:

.. code-block:: ada

  package Overwrite_Record is
     type R is record
        A : Character := 'C';
        B : Character := 'A';
     end record;
     X : Short_Integer := 3;
     Y : R;
     for Y'Address use X'Address;
     pragma Import (Ada, Y);
  end Overwrite_Record;


Note that the use of ``pragma Initialize_Scalars`` may cause variables to
be initialized when they would not otherwise have been in the absence
of the use of this pragma. This may cause an overlay to have this
unintended clobbering effect. The compiler avoids this for scalar
types, but not for composite objects (where in general the effect
of ``Initialize_Scalars`` is part of the initialization routine
for the composite object:

::

  pragma Initialize_Scalars;
  with Ada.Text_IO;  use Ada.Text_IO;
  procedure Overwrite_Array is
     type Arr is array (1 .. 5) of Integer;
     X : Arr := (others => 1);
     A : Arr;
     for A'Address use X'Address;
         |
  >>> warning: default initialization of "A" may
      modify "X", use pragma Import for "A" to
      suppress initialization (RM B.1(24))

  begin
     if X /= Arr'(others => 1) then
        Put_Line ("X was clobbered");
     else
        Put_Line ("X was not clobbered");
     end if;
  end Overwrite_Array;

The above program generates the warning as shown, and at execution
time, prints ``X was clobbered``. If the ``pragma Import`` is
added as suggested:

.. code-block:: ada

  pragma Initialize_Scalars;
  with Ada.Text_IO;  use Ada.Text_IO;
  procedure Overwrite_Array is
     type Arr is array (1 .. 5) of Integer;
     X : Arr := (others => 1);
     A : Arr;
     for A'Address use X'Address;
     pragma Import (Ada, A);
  begin
     if X /= Arr'(others => 1) then
        Put_Line ("X was clobbered");
     else
        Put_Line ("X was not clobbered");
     end if;
  end Overwrite_Array;

then the program compiles without the warning and when run will generate
the output ``X was not clobbered``.


.. _Use_of_Address_Clauses_for_Memory-Mapped_I/O:

Use of Address Clauses for Memory-Mapped I/O
============================================

.. index:: Memory-mapped I/O

A common pattern is to use an address clause to map an atomic variable to
a location in memory that corresponds to a memory-mapped I/O operation or
operations, for example:

.. code-block:: ada

      type Mem_Word is record
         A,B,C,D : Byte;
      end record;
      pragma Atomic (Mem_Word);
      for Mem_Word_Size use 32;

      Mem : Mem_Word;
      for Mem'Address use some-address;
      ...
      Temp := Mem;
      Temp.A := 32;
      Mem := Temp;

For a full access (reference or modification) of the variable (Mem) in this
case, as in the above examples, GNAT guarantees that the entire atomic word
will be accessed, in accordance with the RM C.6(15) clause.

A problem arises with a component access such as:

.. code-block:: ada

      Mem.A := 32;

Note that the component A is not declared as atomic. This means that it is
not clear what this assignment means. It could correspond to full word read
and write as given in the first example, or on architectures that supported
such an operation it might be a single byte store instruction. The RM does
not have anything to say in this situation, and GNAT does not make any
guarantee. The code generated may vary from target to target. GNAT will issue
a warning in such a case:

::

      Mem.A := 32;
      |
      >>> warning: access to non-atomic component of atomic array,
          may cause unexpected accesses to atomic object

It is best to be explicit in this situation, by either declaring the
components to be atomic if you want the byte store, or explicitly writing
the full word access sequence if that is what the hardware requires.
Alternatively, if the full word access sequence is required, GNAT also
provides the pragma ``Volatile_Full_Access`` which can be used in lieu of
pragma ``Atomic`` and will give the additional guarantee.


.. _Effect_of_Convention_on_Representation:

Effect of Convention on Representation
======================================

.. index:: Convention, effect on representation

Normally the specification of a foreign language convention for a type or
an object has no effect on the chosen representation.  In particular, the
representation chosen for data in GNAT generally meets the standard system
conventions, and for example records are laid out in a manner that is
consistent with C.  This means that specifying convention C (for example)
has no effect.

There are four exceptions to this general rule:

* *Convention Fortran and array subtypes*.

  If pragma Convention Fortran is specified for an array subtype, then in
  accordance with the implementation advice in section 3.6.2(11) of the
  Ada Reference Manual, the array will be stored in a Fortran-compatible
  column-major manner, instead of the normal default row-major order.

* *Convention C and enumeration types*

  GNAT normally stores enumeration types in 8, 16, or 32 bits as required
  to accommodate all values of the type.  For example, for the enumeration
  type declared by:

  ::

       type Color is (Red, Green, Blue);

  8 bits is sufficient to store all values of the type, so by default, objects
  of type ``Color`` will be represented using 8 bits.  However, normal C
  convention is to use 32 bits for all enum values in C, since enum values
  are essentially of type int.  If pragma ``Convention C`` is specified for an
  Ada enumeration type, then the size is modified as necessary (usually to
  32 bits) to be consistent with the C convention for enum values.

  Note that this treatment applies only to types. If Convention C is given for
  an enumeration object, where the enumeration type is not Convention C, then
  Object_Size bits are allocated. For example, for a normal enumeration type,
  with less than 256 elements, only 8 bits will be allocated for the object.
  Since this may be a surprise in terms of what C expects, GNAT will issue a
  warning in this situation. The warning can be suppressed by giving an explicit
  size clause specifying the desired size.

* *Convention C/Fortran and Boolean types*

  In C, the usual convention for boolean values, that is values used for
  conditions, is that zero represents false, and nonzero values represent
  true.  In Ada, the normal convention is that two specific values, typically
  0/1, are used to represent false/true respectively.

  Fortran has a similar convention for ``LOGICAL`` values (any nonzero
  value represents true).

  To accommodate the Fortran and C conventions, if a pragma Convention specifies
  C or Fortran convention for a derived Boolean, as in the following example:

  ::

       type C_Switch is new Boolean;
       pragma Convention (C, C_Switch);


  then the GNAT generated code will treat any nonzero value as true.  For truth
  values generated by GNAT, the conventional value 1 will be used for True, but
  when one of these values is read, any nonzero value is treated as True.


.. _Conventions_and_Anonymous_Access_Types:

Conventions and Anonymous Access Types
======================================

.. index:: Anonymous access types

.. index:: Convention for anonymous access types

The RM is not entirely clear on convention handling in a number of cases,
and in particular, it is not clear on the convention to be given to
anonymous access types in general, and in particular what is to be
done for the case of anonymous access-to-subprogram.

In GNAT, we decide that if an explicit Convention is applied
to an object or component, and its type is such an anonymous type,
then the convention will apply to this anonymous type as well. This
seems to make sense since it is anomolous in any case to have a
different convention for an object and its type, and there is clearly
no way to explicitly specify a convention for an anonymous type, since
it doesn't have a name to specify!

Furthermore, we decide that if a convention is applied to a record type,
then this convention is inherited by any of its components that are of an
anonymous access type which do not have an explicitly specified convention.

The following program shows these conventions in action:

::

  package ConvComp is
     type Foo is range 1 .. 10;
     type T1 is record
        A : access function (X : Foo) return Integer;
        B : Integer;
     end record;
     pragma Convention (C, T1);

     type T2 is record
        A : access function (X : Foo) return Integer;
        pragma Convention  (C, A);
        B : Integer;
     end record;
     pragma Convention (COBOL, T2);

     type T3 is record
        A : access function (X : Foo) return Integer;
        pragma Convention  (COBOL, A);
        B : Integer;
     end record;
     pragma Convention (C, T3);

     type T4 is record
        A : access function (X : Foo) return Integer;
        B : Integer;
     end record;
     pragma Convention (COBOL, T4);

     function F (X : Foo) return Integer;
     pragma Convention (C, F);

     function F (X : Foo) return Integer is (13);

     TV1 : T1 := (F'Access, 12);  -- OK
     TV2 : T2 := (F'Access, 13);  -- OK

     TV3 : T3 := (F'Access, 13);  -- ERROR
                  |
  >>> subprogram "F" has wrong convention
  >>> does not match access to subprogram declared at line 17
       38.    TV4 : T4 := (F'Access, 13);  -- ERROR
                  |
  >>> subprogram "F" has wrong convention
  >>> does not match access to subprogram declared at line 24
       39. end ConvComp;


.. _Determining_the_Representations_chosen_by_GNAT:

Determining the Representations chosen by GNAT
==============================================

.. index:: Representation, determination of

.. index:: -gnatR (gcc)

Although the descriptions in this section are intended to be complete, it is
often easier to simply experiment to see what GNAT accepts and what the
effect is on the layout of types and objects.

As required by the Ada RM, if a representation clause is not accepted, then
it must be rejected as illegal by the compiler.  However, when a
representation clause or pragma is accepted, there can still be questions
of what the compiler actually does.  For example, if a partial record
representation clause specifies the location of some components and not
others, then where are the non-specified components placed? Or if pragma
``Pack`` is used on a record, then exactly where are the resulting
fields placed? The section on pragma ``Pack`` in this chapter can be
used to answer the second question, but it is often easier to just see
what the compiler does.

For this purpose, GNAT provides the option *-gnatR*.  If you compile
with this option, then the compiler will output information on the actual
representations chosen, in a format similar to source representation
clauses.  For example, if we compile the package:

.. code-block:: ada

  package q is
     type r (x : boolean) is tagged record
        case x is
           when True => S : String (1 .. 100);
           when False => null;
        end case;
     end record;

     type r2 is new r (false) with record
        y2 : integer;
     end record;

     for r2 use record
        y2 at 16 range 0 .. 31;
     end record;

     type x is record
        y : character;
     end record;

     type x1 is array (1 .. 10) of x;
     for x1'component_size use 11;

     type ia is access integer;

     type Rb1 is array (1 .. 13) of Boolean;
     pragma Pack (rb1);

     type Rb2 is array (1 .. 65) of Boolean;
     pragma Pack (rb2);

     type x2 is record
        l1 : Boolean;
        l2 : Duration;
        l3 : Float;
        l4 : Boolean;
        l5 : Rb1;
        l6 : Rb2;
     end record;
     pragma Pack (x2);
  end q;

using the switch *-gnatR* we obtain the following output:

.. code-block:: ada

  Representation information for unit q
  -------------------------------------

  for r'Size use ??;
  for r'Alignment use 4;
  for r use record
     x    at 4 range  0 .. 7;
     _tag at 0 range  0 .. 31;
     s    at 5 range  0 .. 799;
  end record;

  for r2'Size use 160;
  for r2'Alignment use 4;
  for r2 use record
     x       at  4 range  0 .. 7;
     _tag    at  0 range  0 .. 31;
     _parent at  0 range  0 .. 63;
     y2      at 16 range  0 .. 31;
  end record;

  for x'Size use 8;
  for x'Alignment use 1;
  for x use record
     y at 0 range  0 .. 7;
  end record;

  for x1'Size use 112;
  for x1'Alignment use 1;
  for x1'Component_Size use 11;

  for rb1'Size use 13;
  for rb1'Alignment use 2;
  for rb1'Component_Size use 1;

  for rb2'Size use 72;
  for rb2'Alignment use 1;
  for rb2'Component_Size use 1;

  for x2'Size use 224;
  for x2'Alignment use 4;
  for x2 use record
     l1 at  0 range  0 .. 0;
     l2 at  0 range  1 .. 64;
     l3 at 12 range  0 .. 31;
     l4 at 16 range  0 .. 0;
     l5 at 16 range  1 .. 13;
     l6 at 18 range  0 .. 71;
  end record;

The Size values are actually the Object_Size, i.e., the default size that
will be allocated for objects of the type.
The ``??`` size for type r indicates that we have a variant record, and the
actual size of objects will depend on the discriminant value.

The Alignment values show the actual alignment chosen by the compiler
for each record or array type.

The record representation clause for type r shows where all fields
are placed, including the compiler generated tag field (whose location
cannot be controlled by the programmer).

The record representation clause for the type extension r2 shows all the
fields present, including the parent field, which is a copy of the fields
of the parent type of r2, i.e., r1.

The component size and size clauses for types rb1 and rb2 show
the exact effect of pragma ``Pack`` on these arrays, and the record
representation clause for type x2 shows how pragma `Pack` affects
this record type.

In some cases, it may be useful to cut and paste the representation clauses
generated by the compiler into the original source to fix and guarantee
the actual representation to be used.
