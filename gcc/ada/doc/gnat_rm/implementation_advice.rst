.. _Implementation_Advice:

*********************
Implementation Advice
*********************

The main text of the Ada Reference Manual describes the required
behavior of all Ada compilers, and the GNAT compiler conforms to
these requirements.

In addition, there are sections throughout the Ada Reference Manual headed
by the phrase 'Implementation advice'.  These sections are not normative,
i.e., they do not specify requirements that all compilers must
follow.  Rather they provide advice on generally desirable behavior.
They are not requirements, because they describe behavior that cannot
be provided on all systems, or may be undesirable on some systems.

As far as practical, GNAT follows the implementation advice in
the Ada Reference Manual.  Each such RM section corresponds to a section
in this chapter whose title specifies the
RM section number and paragraph number and the subject of
the advice.  The contents of each section consists of the RM text within
quotation marks,
followed by the GNAT interpretation of the advice.  Most often, this simply says
'followed', which means that GNAT follows the advice.  However, in a
number of cases, GNAT deliberately deviates from this advice, in which
case the text describes what GNAT does and why.

.. index:: Error detection

RM 1.1.3(20): Error Detection
=============================

  "If an implementation detects the use of an unsupported Specialized Needs
  Annex feature at run time, it should raise ``Program_Error`` if
  feasible."

Not relevant.  All specialized needs annex features are either supported,
or diagnosed at compile time.

.. index:: Child Units

RM 1.1.3(31): Child Units
=========================


  "If an implementation wishes to provide implementation-defined
  extensions to the functionality of a language-defined library unit, it
  should normally do so by adding children to the library unit."

Followed.

.. index:: Bounded errors

RM 1.1.5(12): Bounded Errors
============================

  "If an implementation detects a bounded error or erroneous
  execution, it should raise ``Program_Error``."

Followed in all cases in which the implementation detects a bounded
error or erroneous execution.  Not all such situations are detected at
runtime.

.. index:: Pragmas

.. _RM_2_8_16_Pragmas:

RM 2.8(16): Pragmas
===================

  "Normally, implementation-defined pragmas should have no semantic effect
  for error-free programs; that is, if the implementation-defined pragmas
  are removed from a working program, the program should still be legal,
  and should still have the same semantics."

The following implementation defined pragmas are exceptions to this
rule:

+--------------------+-------------------+
| Pragma             | Explanation       |
+====================+===================+
| *Abort_Defer*      | Affects semantics |
+--------------------+-------------------+
|*Ada_83*            | Affects legality  |
+--------------------+-------------------+
|*Assert*            | Affects semantics |
+--------------------+-------------------+
|*CPP_Class*         | Affects semantics |
+--------------------+-------------------+
|*CPP_Constructor*   | Affects semantics |
+--------------------+-------------------+
|*Debug*             | Affects semantics |
+--------------------+-------------------+
|*Interface_Name*    | Affects semantics |
+--------------------+-------------------+
|*Machine_Attribute* | Affects semantics |
+--------------------+-------------------+
|*Unimplemented_Unit*| Affects legality  |
+--------------------+-------------------+
|*Unchecked_Union*   | Affects semantics |
+--------------------+-------------------+

In each of the above cases, it is essential to the purpose of the pragma
that this advice not be followed.  For details see
:ref:`Implementation_Defined_Pragmas`.

RM 2.8(17-19): Pragmas
======================

  "Normally, an implementation should not define pragmas that can
  make an illegal program legal, except as follows:

  * A pragma used to complete a declaration, such as a pragma ``Import``;

  * A pragma used to configure the environment by adding, removing, or
    replacing ``library_items``."

See :ref:`RM_2_8_16_Pragmas`.

.. index:: Character Sets

.. index:: Alternative Character Sets

RM 3.5.2(5): Alternative Character Sets
=======================================

  "If an implementation supports a mode with alternative interpretations
  for ``Character`` and ``Wide_Character``, the set of graphic
  characters of ``Character`` should nevertheless remain a proper
  subset of the set of graphic characters of ``Wide_Character``.  Any
  character set 'localizations' should be reflected in the results of
  the subprograms defined in the language-defined package
  ``Characters.Handling`` (see A.3) available in such a mode.  In a mode with
  an alternative interpretation of ``Character``, the implementation should
  also support a corresponding change in what is a legal
  ``identifier_letter``."

Not all wide character modes follow this advice, in particular the JIS
and IEC modes reflect standard usage in Japan, and in these encoding,
the upper half of the Latin-1 set is not part of the wide-character
subset, since the most significant bit is used for wide character
encoding.  However, this only applies to the external forms.  Internally
there is no such restriction.

.. index:: Integer types

RM 3.5.4(28): Integer Types
===========================

  "An implementation should support ``Long_Integer`` in addition to
  ``Integer`` if the target machine supports 32-bit (or longer)
  arithmetic.  No other named integer subtypes are recommended for package
  ``Standard``.  Instead, appropriate named integer subtypes should be
  provided in the library package ``Interfaces`` (see B.2)."

``Long_Integer`` is supported.  Other standard integer types are supported
so this advice is not fully followed.  These types
are supported for convenient interface to C, and so that all hardware
types of the machine are easily available.

RM 3.5.4(29): Integer Types
===========================

  "An implementation for a two's complement machine should support
  modular types with a binary modulus up to ``System.Max_Int*2+2``.  An
  implementation should support a non-binary modules up to ``Integer'Last``."

Followed.

.. index:: Enumeration values

RM 3.5.5(8): Enumeration Values
===============================

  "For the evaluation of a call on ``S'Pos`` for an enumeration
  subtype, if the value of the operand does not correspond to the internal
  code for any enumeration literal of its type (perhaps due to an
  un-initialized variable), then the implementation should raise
  ``Program_Error``.  This is particularly important for enumeration
  types with noncontiguous internal codes specified by an
  enumeration_representation_clause."

Followed.

.. index:: Float types

RM 3.5.7(17): Float Types
=========================

  "An implementation should support ``Long_Float`` in addition to
  ``Float`` if the target machine supports 11 or more digits of
  precision.  No other named floating point subtypes are recommended for
  package ``Standard``.  Instead, appropriate named floating point subtypes
  should be provided in the library package ``Interfaces`` (see B.2)."

``Short_Float`` and ``Long_Long_Float`` are also provided.  The
former provides improved compatibility with other implementations
supporting this type.  The latter corresponds to the highest precision
floating-point type supported by the hardware.  On most machines, this
will be the same as ``Long_Float``, but on some machines, it will
correspond to the IEEE extended form.  The notable case is all x86
implementations, where ``Long_Long_Float`` corresponds to the 80-bit
extended precision format supported in hardware on this processor.
Note that the 128-bit format on SPARC is not supported, since this
is a software rather than a hardware format.

.. index:: Multidimensional arrays

.. index:: Arrays, multidimensional

RM 3.6.2(11): Multidimensional Arrays
=====================================

  "An implementation should normally represent multidimensional arrays in
  row-major order, consistent with the notation used for multidimensional
  array aggregates (see 4.3.3).  However, if a pragma ``Convention``
  (``Fortran``, ...) applies to a multidimensional array type, then
  column-major order should be used instead (see B.5, *Interfacing with Fortran*)."

Followed.

.. index:: Duration'Small

RM 9.6(30-31): Duration'Small
=============================

  "Whenever possible in an implementation, the value of ``Duration'Small``
  should be no greater than 100 microseconds."

Followed.  (``Duration'Small`` = 10**(-9)).

  "The time base for ``delay_relative_statements`` should be monotonic;
  it need not be the same time base as used for ``Calendar.Clock``."

Followed.

RM 10.2.1(12): Consistent Representation
========================================

  "In an implementation, a type declared in a pre-elaborated package should
  have the same representation in every elaboration of a given version of
  the package, whether the elaborations occur in distinct executions of
  the same program, or in executions of distinct programs or partitions
  that include the given version."

Followed, except in the case of tagged types.  Tagged types involve
implicit pointers to a local copy of a dispatch table, and these pointers
have representations which thus depend on a particular elaboration of the
package.  It is not easy to see how it would be possible to follow this
advice without severely impacting efficiency of execution.

.. index:: Exception information

RM 11.4.1(19): Exception Information
====================================

  "``Exception_Message`` by default and ``Exception_Information``
  should produce information useful for
  debugging.  ``Exception_Message`` should be short, about one
  line.  ``Exception_Information`` can be long.  ``Exception_Message``
  should not include the
  ``Exception_Name``.  ``Exception_Information`` should include both
  the ``Exception_Name`` and the ``Exception_Message``."

Followed.  For each exception that doesn't have a specified
``Exception_Message``, the compiler generates one containing the location
of the raise statement.  This location has the form 'file_name:line', where
file_name is the short file name (without path information) and line is the line
number in the file.  Note that in the case of the Zero Cost Exception
mechanism, these messages become redundant with the Exception_Information that
contains a full backtrace of the calling sequence, so they are disabled.
To disable explicitly the generation of the source location message, use the
Pragma ``Discard_Names``.

.. index:: Suppression of checks

.. index:: Checks, suppression of

RM 11.5(28): Suppression of Checks
==================================

  "The implementation should minimize the code executed for checks that
  have been suppressed."

Followed.

.. index:: Representation clauses

RM 13.1 (21-24): Representation Clauses
=======================================

  "The recommended level of support for all representation items is
  qualified as follows:

  An implementation need not support representation items containing
  nonstatic expressions, except that an implementation should support a
  representation item for a given entity if each nonstatic expression in
  the representation item is a name that statically denotes a constant
  declared before the entity."

Followed.  In fact, GNAT goes beyond the recommended level of support
by allowing nonstatic expressions in some representation clauses even
without the need to declare constants initialized with the values of
such expressions. For example:

.. code-block:: ada

    X : Integer;
    Y : Float;
    for Y'Address use X'Address;

is accepted directly by GNAT.


  "An implementation need not support a specification for the ``Size``
  for a given composite subtype, nor the size or storage place for an
  object (including a component) of a given composite subtype, unless the
  constraints on the subtype and its composite subcomponents (if any) are
  all static constraints."

Followed.  Size Clauses are not permitted on nonstatic components, as
described above.


  "An aliased component, or a component whose type is by-reference, should
  always be allocated at an addressable location."

Followed.

.. index:: Packed types

RM 13.2(6-8): Packed Types
==========================

  "If a type is packed, then the implementation should try to minimize
  storage allocated to objects of the type, possibly at the expense of
  speed of accessing components, subject to reasonable complexity in
  addressing calculations.

  The recommended level of support pragma ``Pack`` is:

  For a packed record type, the components should be packed as tightly as
  possible subject to the Sizes of the component subtypes, and subject to
  any *record_representation_clause* that applies to the type; the
  implementation may, but need not, reorder components or cross aligned
  word boundaries to improve the packing.  A component whose ``Size`` is
  greater than the word size may be allocated an integral number of words."

Followed.  Tight packing of arrays is supported for all component sizes
up to 64-bits. If the array component size is 1 (that is to say, if
the component is a boolean type or an enumeration type with two values)
then values of the type are implicitly initialized to zero. This
happens both for objects of the packed type, and for objects that have a
subcomponent of the packed type.

.. index:: Address clauses

RM 13.3(14-19): Address Clauses
===============================

  "For an array ``X``, ``X'Address`` should point at the first
  component of the array, and not at the array bounds."

Followed.

  "The recommended level of support for the ``Address`` attribute is:

  ``X'Address`` should produce a useful result if ``X`` is an
  object that is aliased or of a by-reference type, or is an entity whose
  ``Address`` has been specified."

Followed.  A valid address will be produced even if none of those
conditions have been met.  If necessary, the object is forced into
memory to ensure the address is valid.

  "An implementation should support ``Address`` clauses for imported
  subprograms."

Followed.

  "Objects (including subcomponents) that are aliased or of a by-reference
  type should be allocated on storage element boundaries."

Followed.

  "If the ``Address`` of an object is specified, or it is imported or exported,
  then the implementation should not perform optimizations based on
  assumptions of no aliases."

Followed.

.. index:: Alignment clauses

RM 13.3(29-35): Alignment Clauses
=================================

  "The recommended level of support for the ``Alignment`` attribute for
  subtypes is:

  An implementation should support specified Alignments that are factors
  and multiples of the number of storage elements per word, subject to the
  following:"

Followed.

  "An implementation need not support specified Alignments for
  combinations of Sizes and Alignments that cannot be easily
  loaded and stored by available machine instructions."

Followed.

  "An implementation need not support specified Alignments that are
  greater than the maximum ``Alignment`` the implementation ever returns by
  default."

Followed.

  "The recommended level of support for the ``Alignment`` attribute for
  objects is:

  Same as above, for subtypes, but in addition:"

Followed.

  "For stand-alone library-level objects of statically constrained
  subtypes, the implementation should support all alignments
  supported by the target linker.  For example, page alignment is likely to
  be supported for such objects, but not for subtypes."

Followed.

.. index:: Size clauses

RM 13.3(42-43): Size Clauses
============================

  "The recommended level of support for the ``Size`` attribute of
  objects is:

  A ``Size`` clause should be supported for an object if the specified
  ``Size`` is at least as large as its subtype's ``Size``, and
  corresponds to a size in storage elements that is a multiple of the
  object's ``Alignment`` (if the ``Alignment`` is nonzero)."

Followed.

RM 13.3(50-56): Size Clauses
============================

  "If the ``Size`` of a subtype is specified, and allows for efficient
  independent addressability (see 9.10) on the target architecture, then
  the ``Size`` of the following objects of the subtype should equal the
  ``Size`` of the subtype:

  Aliased objects (including components)."

Followed.

  "`Size` clause on a composite subtype should not affect the
  internal layout of components."

Followed. But note that this can be overridden by use of the implementation
pragma Implicit_Packing in the case of packed arrays.

  "The recommended level of support for the ``Size`` attribute of subtypes is:

  The ``Size`` (if not specified) of a static discrete or fixed point
  subtype should be the number of bits needed to represent each value
  belonging to the subtype using an unbiased representation, leaving space
  for a sign bit only if the subtype contains negative values.  If such a
  subtype is a first subtype, then an implementation should support a
  specified ``Size`` for it that reflects this representation."

Followed.

  "For a subtype implemented with levels of indirection, the ``Size``
  should include the size of the pointers, but not the size of what they
  point at."

Followed.

.. index:: Component_Size clauses

RM 13.3(71-73): Component Size Clauses
======================================

  "The recommended level of support for the ``Component_Size``
  attribute is:

  An implementation need not support specified ``Component_Sizes`` that are
  less than the ``Size`` of the component subtype."

Followed.

  "An implementation should support specified Component_Sizes that
  are factors and multiples of the word size.  For such
  Component_Sizes, the array should contain no gaps between
  components.  For other Component_Sizes (if supported), the array
  should contain no gaps between components when packing is also
  specified; the implementation should forbid this combination in cases
  where it cannot support a no-gaps representation."

Followed.

.. index:: Enumeration representation clauses

.. index:: Representation clauses, enumeration

RM 13.4(9-10): Enumeration Representation Clauses
=================================================

  "The recommended level of support for enumeration representation clauses
  is:

  An implementation need not support enumeration representation clauses
  for boolean types, but should at minimum support the internal codes in
  the range ``System.Min_Int .. System.Max_Int``."

Followed.

.. index:: Record representation clauses

.. index:: Representation clauses, records

RM 13.5.1(17-22): Record Representation Clauses
===============================================

  "The recommended level of support for
  *record_representation_clause*\ s is:

  An implementation should support storage places that can be extracted
  with a load, mask, shift sequence of machine code, and set with a load,
  shift, mask, store sequence, given the available machine instructions
  and run-time model."

Followed.

  "A storage place should be supported if its size is equal to the
  ``Size`` of the component subtype, and it starts and ends on a
  boundary that obeys the ``Alignment`` of the component subtype."

Followed.

  "If the default bit ordering applies to the declaration of a given type,
  then for a component whose subtype's ``Size`` is less than the word
  size, any storage place that does not cross an aligned word boundary
  should be supported."

Followed.

  "An implementation may reserve a storage place for the tag field of a
  tagged type, and disallow other components from overlapping that place."

Followed.  The storage place for the tag field is the beginning of the tagged
record, and its size is Address'Size.  GNAT will reject an explicit component
clause for the tag field.

  "An implementation need not support a *component_clause* for a
  component of an extension part if the storage place is not after the
  storage places of all components of the parent type, whether or not
  those storage places had been specified."

Followed.  The above advice on record representation clauses is followed,
and all mentioned features are implemented.

.. index:: Storage place attributes

RM 13.5.2(5): Storage Place Attributes
======================================

  "If a component is represented using some form of pointer (such as an
  offset) to the actual data of the component, and this data is contiguous
  with the rest of the object, then the storage place attributes should
  reflect the place of the actual data, not the pointer.  If a component is
  allocated discontinuously from the rest of the object, then a warning
  should be generated upon reference to one of its storage place
  attributes."

Followed.  There are no such components in GNAT.

.. index:: Bit ordering

RM 13.5.3(7-8): Bit Ordering
============================

  "The recommended level of support for the non-default bit ordering is:

  The implementation should support the nondefault bit ordering in addition
  to the default bit ordering."

Followed.

.. index:: Address, as private type

RM 13.7(37): Address as Private
===============================

  "`Address` should be of a private type."

Followed.

.. index:: Operations, on ``Address``

.. index:: Address, operations of

RM 13.7.1(16): Address Operations
=================================

  "Operations in ``System`` and its children should reflect the target
  environment semantics as closely as is reasonable.  For example, on most
  machines, it makes sense for address arithmetic to 'wrap around'.
  Operations that do not make sense should raise ``Program_Error``."

Followed.  Address arithmetic is modular arithmetic that wraps around.  No
operation raises ``Program_Error``, since all operations make sense.

.. index:: Unchecked conversion

RM 13.9(14-17): Unchecked Conversion
====================================

  "The ``Size`` of an array object should not include its bounds; hence,
  the bounds should not be part of the converted data."

Followed.

  "The implementation should not generate unnecessary run-time checks to
  ensure that the representation of ``S`` is a representation of the
  target type.  It should take advantage of the permission to return by
  reference when possible.  Restrictions on unchecked conversions should be
  avoided unless required by the target environment."

Followed.  There are no restrictions on unchecked conversion.  A warning is
generated if the source and target types do not have the same size since
the semantics in this case may be target dependent.

  "The recommended level of support for unchecked conversions is:

  Unchecked conversions should be supported and should be reversible in
  the cases where this clause defines the result.  To enable meaningful use
  of unchecked conversion, a contiguous representation should be used for
  elementary subtypes, for statically constrained array subtypes whose
  component subtype is one of the subtypes described in this paragraph,
  and for record subtypes without discriminants whose component subtypes
  are described in this paragraph."

Followed.

.. index:: Heap usage, implicit

RM 13.11(23-25): Implicit Heap Usage
====================================

  "An implementation should document any cases in which it dynamically
  allocates heap storage for a purpose other than the evaluation of an
  allocator."

Followed, the only other points at which heap storage is dynamically
allocated are as follows:

*
  At initial elaboration time, to allocate dynamically sized global
  objects.

*
  To allocate space for a task when a task is created.

*
  To extend the secondary stack dynamically when needed.  The secondary
  stack is used for returning variable length results.

..

  "A default (implementation-provided) storage pool for an
  access-to-constant type should not have overhead to support deallocation of
  individual objects."

Followed.

  "A storage pool for an anonymous access type should be created at the
  point of an allocator for the type, and be reclaimed when the designated
  object becomes inaccessible."

Followed.

.. index:: Unchecked deallocation

RM 13.11.2(17): Unchecked Deallocation
======================================

  "For a standard storage pool, ``Free`` should actually reclaim the
  storage."

Followed.

.. index:: Stream oriented attributes

RM 13.13.2(1.6): Stream Oriented Attributes
===========================================

  "If not specified, the value of Stream_Size for an elementary type
  should be the number of bits that corresponds to the minimum number of
  stream elements required by the first subtype of the type, rounded up
  to the nearest factor or multiple of the word size that is also a
  multiple of the stream element size."

Followed, except that the number of stream elements is 1, 2, 3, 4 or 8.
The Stream_Size may be used to override the default choice.

The default implementation is based on direct binary representations and is
therefore target- and endianness-dependent.  To address this issue, GNAT also
supplies an alternate implementation of the stream attributes ``Read`` and
``Write``, which uses the target-independent XDR standard representation for
scalar types. This XDR alternative can be enabled via the binder switch -xdr.

.. index:: XDR representation
.. index:: Read attribute
.. index:: Write attribute
.. index:: Stream oriented attributes

RM A.1(52): Names of Predefined Numeric Types
=============================================

  "If an implementation provides additional named predefined integer types,
  then the names should end with ``Integer`` as in
  ``Long_Integer``.  If an implementation provides additional named
  predefined floating point types, then the names should end with
  ``Float`` as in ``Long_Float``."

Followed.

.. index:: Ada.Characters.Handling

RM A.3.2(49): ``Ada.Characters.Handling``
=========================================

  "If an implementation provides a localized definition of ``Character``
  or ``Wide_Character``, then the effects of the subprograms in
  ``Characters.Handling`` should reflect the localizations.
  See also 3.5.2."

Followed.  GNAT provides no such localized definitions.

.. index:: Bounded-length strings

RM A.4.4(106): Bounded-Length String Handling
=============================================

  "Bounded string objects should not be implemented by implicit pointers
  and dynamic allocation."

Followed.  No implicit pointers or dynamic allocation are used.

.. index:: Random number generation

RM A.5.2(46-47): Random Number Generation
=========================================

  "Any storage associated with an object of type ``Generator`` should be
  reclaimed on exit from the scope of the object."

Followed.

  "If the generator period is sufficiently long in relation to the number
  of distinct initiator values, then each possible value of
  ``Initiator`` passed to ``Reset`` should initiate a sequence of
  random numbers that does not, in a practical sense, overlap the sequence
  initiated by any other value.  If this is not possible, then the mapping
  between initiator values and generator states should be a rapidly
  varying function of the initiator value."

Followed.  The generator period is sufficiently long for the first
condition here to hold true.

.. index:: Get_Immediate

RM A.10.7(23): ``Get_Immediate``
================================

  "The ``Get_Immediate`` procedures should be implemented with
  unbuffered input.  For a device such as a keyboard, input should be
  available if a key has already been typed, whereas for a disk
  file, input should always be available except at end of file.  For a file
  associated with a keyboard-like device, any line-editing features of the
  underlying operating system should be disabled during the execution of
  ``Get_Immediate``."

Followed on all targets except VxWorks. For VxWorks, there is no way to
provide this functionality that does not result in the input buffer being
flushed before the ``Get_Immediate`` call. A special unit
``Interfaces.Vxworks.IO`` is provided that contains routines to enable
this functionality.

.. index:: Containers

RM A.18: ``Containers``
================================

All implementation advice pertaining to Ada.Containers and its
child units (that is, all implementation advice occurring within
section A.18 and its subsections) is followed except for A.18.24(17):

   "Bounded ordered set objects should be implemented without implicit pointers or dynamic allocation. "

The implementations of the two Reference_Preserving_Key functions of
the generic package Ada.Containers.Bounded_Ordered_Sets each currently make
use of dynamic allocation; other operations on bounded ordered set objects
follow the implementation advice.

.. index:: Export

RM B.1(39-41): Pragma ``Export``
================================

  "If an implementation supports pragma ``Export`` to a given language,
  then it should also allow the main subprogram to be written in that
  language.  It should support some mechanism for invoking the elaboration
  of the Ada library units included in the system, and for invoking the
  finalization of the environment task.  On typical systems, the
  recommended mechanism is to provide two subprograms whose link names are
  ``adainit`` and ``adafinal``.  ``adainit`` should contain the
  elaboration code for library units.  ``adafinal`` should contain the
  finalization code.  These subprograms should have no effect the second
  and subsequent time they are called."

Followed.

  "Automatic elaboration of pre-elaborated packages should be
  provided when pragma ``Export`` is supported."

Followed when the main program is in Ada.  If the main program is in a
foreign language, then
``adainit`` must be called to elaborate pre-elaborated
packages.

  "For each supported convention *L* other than ``Intrinsic``, an
  implementation should support ``Import`` and ``Export`` pragmas
  for objects of *L*\ -compatible types and for subprograms, and pragma
  `Convention` for *L*\ -eligible types and for subprograms,
  presuming the other language has corresponding features.  Pragma
  ``Convention`` need not be supported for scalar types."

Followed.

.. index:: Package Interfaces

.. index:: Interfaces

RM B.2(12-13): Package ``Interfaces``
=====================================

  "For each implementation-defined convention identifier, there should be a
  child package of package Interfaces with the corresponding name.  This
  package should contain any declarations that would be useful for
  interfacing to the language (implementation) represented by the
  convention.  Any declarations useful for interfacing to any language on
  the given hardware architecture should be provided directly in
  ``Interfaces``."

Followed.

  "An implementation supporting an interface to C, COBOL, or Fortran should
  provide the corresponding package or packages described in the following
  clauses."

Followed.  GNAT provides all the packages described in this section.

.. index:: C, interfacing with

RM B.3(63-71): Interfacing with C
=================================

  "An implementation should support the following interface correspondences
  between Ada and C."

Followed.

  "An Ada procedure corresponds to a void-returning C function."

Followed.

  "An Ada function corresponds to a non-void C function."

Followed.

  "An Ada ``in`` scalar parameter is passed as a scalar argument to a C
  function."

Followed.

  "An Ada ``in`` parameter of an access-to-object type with designated
  type ``T`` is passed as a ``t*`` argument to a C function,
  where ``t`` is the C type corresponding to the Ada type ``T``."

Followed.

  "An Ada access ``T`` parameter, or an Ada ``out`` or ``in out``
  parameter of an elementary type ``T``, is passed as a ``t*``
  argument to a C function, where ``t`` is the C type corresponding to
  the Ada type ``T``.  In the case of an elementary ``out`` or
  ``in out`` parameter, a pointer to a temporary copy is used to
  preserve by-copy semantics."

Followed.

  "An Ada parameter of a record type ``T``, of any mode, is passed as a
  ``t*`` argument to a C function, where ``t`` is the C
  structure corresponding to the Ada type ``T``."

Followed.  This convention may be overridden by the use of the C_Pass_By_Copy
pragma, or Convention, or by explicitly specifying the mechanism for a given
call using an extended import or export pragma.

  "An Ada parameter of an array type with component type ``T``, of any
  mode, is passed as a ``t*`` argument to a C function, where
  ``t`` is the C type corresponding to the Ada type ``T``."

Followed.

  "An Ada parameter of an access-to-subprogram type is passed as a pointer
  to a C function whose prototype corresponds to the designated
  subprogram's specification."

Followed.

.. index:: COBOL, interfacing with

RM B.4(95-98): Interfacing with COBOL
=====================================

  "An Ada implementation should support the following interface
  correspondences between Ada and COBOL."

Followed.

  "An Ada access ``T`` parameter is passed as a ``BY REFERENCE`` data item of
  the COBOL type corresponding to ``T``."

Followed.

  "An Ada in scalar parameter is passed as a ``BY CONTENT`` data item of
  the corresponding COBOL type."

Followed.

  "Any other Ada parameter is passed as a ``BY REFERENCE`` data item of the
  COBOL type corresponding to the Ada parameter type; for scalars, a local
  copy is used if necessary to ensure by-copy semantics."

Followed.

.. index:: Fortran, interfacing with

RM B.5(22-26): Interfacing with Fortran
=======================================

  "An Ada implementation should support the following interface
  correspondences between Ada and Fortran:"

Followed.

  "An Ada procedure corresponds to a Fortran subroutine."

Followed.

  "An Ada function corresponds to a Fortran function."

Followed.

  "An Ada parameter of an elementary, array, or record type ``T`` is
  passed as a ``T`` argument to a Fortran procedure, where ``T`` is
  the Fortran type corresponding to the Ada type ``T``, and where the
  INTENT attribute of the corresponding dummy argument matches the Ada
  formal parameter mode; the Fortran implementation's parameter passing
  conventions are used.  For elementary types, a local copy is used if
  necessary to ensure by-copy semantics."

Followed.

  "An Ada parameter of an access-to-subprogram type is passed as a
  reference to a Fortran procedure whose interface corresponds to the
  designated subprogram's specification."

Followed.

.. index:: Machine operations

RM C.1(3-5): Access to Machine Operations
=========================================

  "The machine code or intrinsic support should allow access to all
  operations normally available to assembly language programmers for the
  target environment, including privileged instructions, if any."

Followed.

  "The interfacing pragmas (see Annex B) should support interface to
  assembler; the default assembler should be associated with the
  convention identifier ``Assembler``."

Followed.

  "If an entity is exported to assembly language, then the implementation
  should allocate it at an addressable location, and should ensure that it
  is retained by the linking process, even if not otherwise referenced
  from the Ada code.  The implementation should assume that any call to a
  machine code or assembler subprogram is allowed to read or update every
  object that is specified as exported."

Followed.

RM C.1(10-16): Access to Machine Operations
===========================================

  "The implementation should ensure that little or no overhead is
  associated with calling intrinsic and machine-code subprograms."

Followed for both intrinsics and machine-code subprograms.

  "It is recommended that intrinsic subprograms be provided for convenient
  access to any machine operations that provide special capabilities or
  efficiency and that are not otherwise available through the language
  constructs."

Followed.  A full set of machine operation intrinsic subprograms is provided.

  "Atomic read-modify-write operations---e.g., test and set, compare and
  swap, decrement and test, enqueue/dequeue."

Followed on any target supporting such operations.

  "Standard numeric functions---e.g.:, sin, log."

Followed on any target supporting such operations.

  "String manipulation operations---e.g.:, translate and test."

Followed on any target supporting such operations.

  "Vector operations---e.g.:, compare vector against thresholds."

Followed on any target supporting such operations.

  "Direct operations on I/O ports."

Followed on any target supporting such operations.

.. index:: Interrupt support

RM C.3(28): Interrupt Support
=============================

  "If the ``Ceiling_Locking`` policy is not in effect, the
  implementation should provide means for the application to specify which
  interrupts are to be blocked during protected actions, if the underlying
  system allows for a finer-grain control of interrupt blocking."

Followed.  The underlying system does not allow for finer-grain control
of interrupt blocking.

.. index:: Protected procedure handlers

RM C.3.1(20-21): Protected Procedure Handlers
=============================================

  "Whenever possible, the implementation should allow interrupt handlers to
  be called directly by the hardware."

Followed on any target where the underlying operating system permits
such direct calls.

  "Whenever practical, violations of any
  implementation-defined restrictions should be detected before run time."

Followed.  Compile time warnings are given when possible.

.. index:: Package ``Interrupts``

.. index:: Interrupts

RM C.3.2(25): Package ``Interrupts``
====================================

  "If implementation-defined forms of interrupt handler procedures are
  supported, such as protected procedures with parameters, then for each
  such form of a handler, a type analogous to ``Parameterless_Handler``
  should be specified in a child package of ``Interrupts``, with the
  same operations as in the predefined package Interrupts."

Followed.

.. index:: Pre-elaboration requirements

RM C.4(14): Pre-elaboration Requirements
========================================

  "It is recommended that pre-elaborated packages be implemented in such a
  way that there should be little or no code executed at run time for the
  elaboration of entities not already covered by the Implementation
  Requirements."

Followed.  Executable code is generated in some cases, e.g., loops
to initialize large arrays.

RM C.5(8): Pragma ``Discard_Names``
===================================

  "If the pragma applies to an entity, then the implementation should
  reduce the amount of storage used for storing names associated with that
  entity."

Followed.

.. index:: Package Task_Attributes

.. index:: Task_Attributes

RM C.7.2(30): The Package Task_Attributes
=========================================

  "Some implementations are targeted to domains in which memory use at run
  time must be completely deterministic.  For such implementations, it is
  recommended that the storage for task attributes will be pre-allocated
  statically and not from the heap.  This can be accomplished by either
  placing restrictions on the number and the size of the task's
  attributes, or by using the pre-allocated storage for the first ``N``
  attribute objects, and the heap for the others.  In the latter case,
  ``N`` should be documented."

Not followed.  This implementation is not targeted to such a domain.

.. index:: Locking Policies

RM D.3(17): Locking Policies
============================

  "The implementation should use names that end with ``_Locking`` for
  locking policies defined by the implementation."

Followed.  Two implementation-defined locking policies are defined,
whose names (``Inheritance_Locking`` and
``Concurrent_Readers_Locking``) follow this suggestion.

.. index:: Entry queuing policies

RM D.4(16): Entry Queuing Policies
==================================

  "Names that end with ``_Queuing`` should be used
  for all implementation-defined queuing policies."

Followed.  No such implementation-defined queuing policies exist.

.. index:: Preemptive abort

RM D.6(9-10): Preemptive Abort
==============================

  "Even though the *abort_statement* is included in the list of
  potentially blocking operations (see 9.5.1), it is recommended that this
  statement be implemented in a way that never requires the task executing
  the *abort_statement* to block."

Followed.

  "On a multi-processor, the delay associated with aborting a task on
  another processor should be bounded; the implementation should use
  periodic polling, if necessary, to achieve this."

Followed.

.. index:: Tasking restrictions

RM D.7(21): Tasking Restrictions
================================

  "When feasible, the implementation should take advantage of the specified
  restrictions to produce a more efficient implementation."

GNAT currently takes advantage of these restrictions by providing an optimized
run time when the Ravenscar profile and the GNAT restricted run time set
of restrictions are specified.  See pragma ``Profile (Ravenscar)`` and
pragma ``Profile (Restricted)`` for more details.

.. index:: Time, monotonic

RM D.8(47-49): Monotonic Time
=============================

  "When appropriate, implementations should provide configuration
  mechanisms to change the value of ``Tick``."

Such configuration mechanisms are not appropriate to this implementation
and are thus not supported.

  "It is recommended that ``Calendar.Clock`` and ``Real_Time.Clock``
  be implemented as transformations of the same time base."

Followed.


  "It is recommended that the best time base which exists in
  the underlying system be available to the application through
  ``Clock``.  `Best` may mean highest accuracy or largest range."

Followed.

.. index:: Partition communication subsystem

.. index:: PCS

RM E.5(28-29): Partition Communication Subsystem
================================================

  "Whenever possible, the PCS on the called partition should allow for
  multiple tasks to call the RPC-receiver with different messages and
  should allow them to block until the corresponding subprogram body
  returns."

Followed by GLADE, a separately supplied PCS that can be used with
GNAT.

  "The ``Write`` operation on a stream of type ``Params_Stream_Type``
  should raise ``Storage_Error`` if it runs out of space trying to
  write the ``Item`` into the stream."

Followed by GLADE, a separately supplied PCS that can be used with
GNAT.

.. index:: COBOL support

RM F(7): COBOL Support
======================

  "If COBOL (respectively, C) is widely supported in the target
  environment, implementations supporting the Information Systems Annex
  should provide the child package ``Interfaces.COBOL`` (respectively,
  ``Interfaces.C``) specified in Annex B and should support a
  ``convention_identifier`` of COBOL (respectively, C) in the interfacing
  pragmas (see Annex B), thus allowing Ada programs to interface with
  programs written in that language."

Followed.

.. index:: Decimal radix support

RM F.1(2): Decimal Radix Support
================================

  "Packed decimal should be used as the internal representation for objects
  of subtype ``S`` when ``S``'Machine_Radix = 10."

Not followed.  GNAT ignores ``S``'Machine_Radix and always uses binary
representations.

.. index:: Numerics

RM G: Numerics
==============

  "If Fortran (respectively, C) is widely supported in the target
  environment, implementations supporting the Numerics Annex
  should provide the child package ``Interfaces.Fortran`` (respectively,
  ``Interfaces.C``) specified in Annex B and should support a
  ``convention_identifier`` of Fortran (respectively, C) in the interfacing
  pragmas (see Annex B), thus allowing Ada programs to interface with
  programs written in that language."

Followed.

.. index:: Complex types

RM G.1.1(56-58): Complex Types
==============================

  "Because the usual mathematical meaning of multiplication of a complex
  operand and a real operand is that of the scaling of both components of
  the former by the latter, an implementation should not perform this
  operation by first promoting the real operand to complex type and then
  performing a full complex multiplication.  In systems that, in the
  future, support an Ada binding to IEC 559:1989, the latter technique
  will not generate the required result when one of the components of the
  complex operand is infinite.  (Explicit multiplication of the infinite
  component by the zero component obtained during promotion yields a NaN
  that propagates into the final result.) Analogous advice applies in the
  case of multiplication of a complex operand and a pure-imaginary
  operand, and in the case of division of a complex operand by a real or
  pure-imaginary operand."

Not followed.

  "Similarly, because the usual mathematical meaning of addition of a
  complex operand and a real operand is that the imaginary operand remains
  unchanged, an implementation should not perform this operation by first
  promoting the real operand to complex type and then performing a full
  complex addition.  In implementations in which the ``Signed_Zeros``
  attribute of the component type is ``True`` (and which therefore
  conform to IEC 559:1989 in regard to the handling of the sign of zero in
  predefined arithmetic operations), the latter technique will not
  generate the required result when the imaginary component of the complex
  operand is a negatively signed zero.  (Explicit addition of the negative
  zero to the zero obtained during promotion yields a positive zero.)
  Analogous advice applies in the case of addition of a complex operand
  and a pure-imaginary operand, and in the case of subtraction of a
  complex operand and a real or pure-imaginary operand."

Not followed.

  "Implementations in which ``Real'Signed_Zeros`` is ``True`` should
  attempt to provide a rational treatment of the signs of zero results and
  result components.  As one example, the result of the ``Argument``
  function should have the sign of the imaginary component of the
  parameter ``X`` when the point represented by that parameter lies on
  the positive real axis; as another, the sign of the imaginary component
  of the ``Compose_From_Polar`` function should be the same as
  (respectively, the opposite of) that of the ``Argument`` parameter when that
  parameter has a value of zero and the ``Modulus`` parameter has a
  nonnegative (respectively, negative) value."

Followed.

.. index:: Complex elementary functions

RM G.1.2(49): Complex Elementary Functions
==========================================

  "Implementations in which ``Complex_Types.Real'Signed_Zeros`` is
  ``True`` should attempt to provide a rational treatment of the signs
  of zero results and result components.  For example, many of the complex
  elementary functions have components that are odd functions of one of
  the parameter components; in these cases, the result component should
  have the sign of the parameter component at the origin.  Other complex
  elementary functions have zero components whose sign is opposite that of
  a parameter component at the origin, or is always positive or always
  negative."

Followed.

.. index:: Accuracy requirements

RM G.2.4(19): Accuracy Requirements
===================================

  "The versions of the forward trigonometric functions without a
  ``Cycle`` parameter should not be implemented by calling the
  corresponding version with a ``Cycle`` parameter of
  ``2.0*Numerics.Pi``, since this will not provide the required
  accuracy in some portions of the domain.  For the same reason, the
  version of ``Log`` without a ``Base`` parameter should not be
  implemented by calling the corresponding version with a ``Base``
  parameter of ``Numerics.e``."

Followed.

.. index:: Complex arithmetic accuracy

.. index:: Accuracy, complex arithmetic

RM G.2.6(15): Complex Arithmetic Accuracy
=========================================

  "The version of the ``Compose_From_Polar`` function without a
  ``Cycle`` parameter should not be implemented by calling the
  corresponding version with a ``Cycle`` parameter of
  ``2.0*Numerics.Pi``, since this will not provide the required
  accuracy in some portions of the domain."

Followed.

.. index:: Sequential elaboration policy

RM H.6(15/2): Pragma Partition_Elaboration_Policy
=================================================

  "If the partition elaboration policy is ``Sequential`` and the
  Environment task becomes permanently blocked during elaboration then the
  partition is deadlocked and it is recommended that the partition be
  immediately terminated."

Not followed.
