.. _Implementation_Defined_Aspects:

******************************
Implementation Defined Aspects
******************************

Ada defines (throughout the Ada 2012 reference manual, summarized
in Annex K) a set of aspects that can be specified for certain entities.
These language defined aspects are implemented in GNAT in Ada 2012 mode
and work as described in the Ada 2012 Reference Manual.

In addition, Ada 2012 allows implementations to define additional aspects
whose meaning is defined by the implementation.  GNAT provides
a number of these implementation-defined aspects which can be used
to extend and enhance the functionality of the compiler.  This section of
the GNAT reference manual describes these additional aspects.

Note that any program using these aspects may not be portable to
other compilers (although GNAT implements this set of aspects on all
platforms).  Therefore if portability to other compilers is an important
consideration, you should minimize the use of these aspects.

Note that for many of these aspects, the effect is essentially similar
to the use of a pragma or attribute specification with the same name
applied to the entity. For example, if we write:


.. code-block:: ada

  type R is range 1 .. 100
    with Value_Size => 10;


then the effect is the same as:

.. code-block:: ada

  type R is range 1 .. 100;
  for R'Value_Size use 10;


and if we write:

.. code-block:: ada

  type R is new Integer
    with Shared => True;


then the effect is the same as:

.. code-block:: ada

  type R is new Integer;
  pragma Shared (R);


In the documentation below, such cases are simply marked
as being boolean aspects equivalent to the corresponding pragma
or attribute definition clause.

Aspect Abstract_State
=====================

.. index:: Abstract_State

This aspect is equivalent to pragma `Abstract_State`.

Annotate
========
.. index:: Annotate

There are three forms of this aspect (where ID is an identifier,
and ARG is a general expression).



*Annotate => ID*
  Equivalent to `pragma Annotate (ID, Entity => Name);`


*Annotate => (ID)*
  Equivalent to `pragma Annotate (ID, Entity => Name);`


*Annotate => (ID ,ID {, ARG})*
  Equivalent to `pragma Annotate (ID, ID {, ARG}, Entity => Name);`

Aspect Async_Readers
====================
.. index:: Async_Readers

This boolean aspect is equivalent to pragma `Async_Readers`.

Aspect Async_Writers
====================
.. index:: Async_Writers

This boolean aspect is equivalent to pragma `Async_Writers`.

Aspect Contract_Cases
=====================
.. index:: Contract_Cases

This aspect is equivalent to pragma `Contract_Cases`, the sequence
of clauses being enclosed in parentheses so that syntactically it is an
aggregate.

Aspect Depends
==============
.. index:: Depends

This aspect is equivalent to pragma `Depends`.

Aspect Dimension
================
.. index:: Dimension

The `Dimension` aspect is used to specify the dimensions of a given
subtype of a dimensioned numeric type. The aspect also specifies a symbol
used when doing formatted output of dimensioned quantities. The syntax is::

  with Dimension =>
    ([Symbol =>] SYMBOL, DIMENSION_VALUE {, DIMENSION_Value})

  SYMBOL ::= STRING_LITERAL | CHARACTER_LITERAL

  DIMENSION_VALUE ::=
    RATIONAL
  | others               => RATIONAL
  | DISCRETE_CHOICE_LIST => RATIONAL

  RATIONAL ::= [-] NUMERIC_LITERAL [/ NUMERIC_LITERAL]


This aspect can only be applied to a subtype whose parent type has
a `Dimension_Systen` aspect. The aspect must specify values for
all dimensions of the system. The rational values are the powers of the
corresponding dimensions that are used by the compiler to verify that
physical (numeric) computations are dimensionally consistent. For example,
the computation of a force must result in dimensions (L => 1, M => 1, T => -2).
For further examples of the usage
of this aspect, see package `System.Dim.Mks`.
Note that when the dimensioned type is an integer type, then any
dimension value must be an integer literal.

Aspect Dimension_System
=======================
.. index:: Dimension_System

The `Dimension_System` aspect is used to define a system of
dimensions that will be used in subsequent subtype declarations with
`Dimension` aspects that reference this system. The syntax is::

  with Dimension_System => (DIMENSION {, DIMENSION});

  DIMENSION ::= ([Unit_Name   =>] IDENTIFIER,
                 [Unit_Symbol =>] SYMBOL,
                 [Dim_Symbol  =>] SYMBOL)

  SYMBOL ::= CHARACTER_LITERAL | STRING_LITERAL


This aspect is applied to a type, which must be a numeric derived type
(typically a floating-point type), that
will represent values within the dimension system. Each `DIMENSION`
corresponds to one particular dimension. A maximum of 7 dimensions may
be specified. `Unit_Name` is the name of the dimension (for example
`Meter`). `Unit_Symbol` is the shorthand used for quantities
of this dimension (for example `m` for `Meter`).
`Dim_Symbol` gives
the identification within the dimension system (typically this is a
single letter, e.g. `L` standing for length for unit name `Meter`).
The `Unit_Symbol` is used in formatted output of dimensioned quantities.
The `Dim_Symbol` is used in error messages when numeric operations have
inconsistent dimensions.

GNAT provides the standard definition of the International MKS system in
the run-time package `System.Dim.Mks`. You can easily define
similar packages for cgs units or British units, and define conversion factors
between values in different systems. The MKS system is characterized by the
following aspect:

.. code-block:: ada

     type Mks_Type is new Long_Long_Float with
       Dimension_System => (
         (Unit_Name => Meter,    Unit_Symbol => 'm',   Dim_Symbol => 'L'),
         (Unit_Name => Kilogram, Unit_Symbol => "kg",  Dim_Symbol => 'M'),
         (Unit_Name => Second,   Unit_Symbol => 's',   Dim_Symbol => 'T'),
         (Unit_Name => Ampere,   Unit_Symbol => 'A',   Dim_Symbol => 'I'),
         (Unit_Name => Kelvin,   Unit_Symbol => 'K',   Dim_Symbol => '@'),
         (Unit_Name => Mole,     Unit_Symbol => "mol", Dim_Symbol => 'N'),
         (Unit_Name => Candela,  Unit_Symbol => "cd",  Dim_Symbol => 'J'));


Note that in the above type definition, we use the `at` symbol (``@``) to
represent a theta character (avoiding the use of extended Latin-1
characters in this context).

See section 'Performing Dimensionality Analysis in GNAT' in the GNAT Users
Guide for detailed examples of use of the dimension system.

Aspect Effective_Reads
======================
.. index:: Effective_Reads

This aspect is equivalent to pragma `Effective_Reads`.

Aspect Effective_Writes
=======================
.. index:: Effective_Writes

This aspect is equivalent to pragma `Effective_Writes`.

Aspect Favor_Top_Level
======================
.. index:: Favor_Top_Level

This boolean aspect is equivalent to pragma `Favor_Top_Level`.

Aspect Global
=============
.. index:: Global

This aspect is equivalent to pragma `Global`.

Aspect Initial_Condition
========================
.. index:: Initial_Condition

This aspect is equivalent to pragma `Initial_Condition`.

Aspect Initializes
==================
.. index:: Initializes

This aspect is equivalent to pragma `Initializes`.

Aspect Inline_Always
====================
.. index:: Inline_Always

This boolean aspect is equivalent to pragma `Inline_Always`.

Aspect Invariant
================
.. index:: Invariant

This aspect is equivalent to pragma `Invariant`. It is a
synonym for the language defined aspect `Type_Invariant` except
that it is separately controllable using pragma `Assertion_Policy`.

Aspect Invariant'Class
======================
.. index:: Invariant'Class

This aspect is equivalent to pragma `Type_Invariant_Class`. It is a
synonym for the language defined aspect `Type_Invariant'Class` except
that it is separately controllable using pragma `Assertion_Policy`.

Aspect Iterable
===============
.. index:: Iterable

This aspect provides a light-weight mechanism for loops and quantified
expressions over container types, without the overhead imposed by the tampering
checks of standard Ada 2012 iterators. The value of the aspect is an aggregate
with four named components: `First`, `Next`, `Has_Element`, and `Element` (the
last one being optional). When only 3 components are specified, only the
`for .. in` form of iteration over cursors is available. When all 4 components
are specified, both this form and the `for .. of` form of iteration over
elements are available. The following is a typical example of use:

.. code-block:: ada

  type List is private with
      Iterable => (First        => First_Cursor,
                   Next         => Advance,
                   Has_Element  => Cursor_Has_Element,
                  [Element      => Get_Element]);

* The value denoted by `First` must denote a primitive operation of the
  container type that returns a `Cursor`, which must a be a type declared in
  the container package or visible from it. For example:

.. code-block:: ada

  function First_Cursor (Cont : Container) return Cursor;

* The value of `Next` is a primitive operation of the container type that takes
  both a container and a cursor and yields a cursor. For example:

.. code-block:: ada

  function Advance (Cont : Container; Position : Cursor) return Cursor;

* The value of `Has_Element` is a primitive operation of the container type
  that takes both a container and a cursor and yields a boolean. For example:

.. code-block:: ada

  function Cursor_Has_Element (Cont : Container; Position : Cursor) return Boolean;

* The value of `Element` is a primitive operation of the container type that
  takes both a container and a cursor and yields an `Element_Type`, which must
  be a type declared in the container package or visible from it. For example:

.. code-block:: ada

  function Get_Element (Cont : Container; Position : Cursor) return Element_Type;

This aspect is used in the GNAT-defined formal container packages.

Aspect Linker_Section
=====================
.. index:: Linker_Section

This aspect is equivalent to an `Linker_Section` pragma.

Aspect Lock_Free
================
.. index:: Lock_Free

This boolean aspect is equivalent to pragma `Lock_Free`.

Aspect No_Elaboration_Code_All
==============================
.. index:: No_Elaboration_Code_All

This aspect is equivalent to a `pragma No_Elaboration_Code_All`
statement for a program unit.

Aspect No_Tagged_Streams
========================
.. index:: No_Tagged_Streams

This aspect is equivalent to a `pragma No_Tagged_Streams` with an
argument specifying a root tagged type (thus this aspect can only be
applied to such a type).

Aspect Object_Size
==================
.. index:: Object_Size

This aspect is equivalent to an `Object_Size` attribute definition
clause.

Aspect Obsolescent
==================
.. index:: Obsolsecent

This aspect is equivalent to an `Obsolescent` pragma. Note that the
evaluation of this aspect happens at the point of occurrence, it is not
delayed until the freeze point.

Aspect Part_Of
==============
.. index:: Part_Of

This aspect is equivalent to pragma `Part_Of`.

Aspect Persistent_BSS
=====================
.. index:: Persistent_BSS

This boolean aspect is equivalent to pragma `Persistent_BSS`.

Aspect Predicate
================
.. index:: Predicate

This aspect is equivalent to pragma `Predicate`. It is thus
similar to the language defined aspects `Dynamic_Predicate`
and `Static_Predicate` except that whether the resulting
predicate is static or dynamic is controlled by the form of the
expression. It is also separately controllable using pragma
`Assertion_Policy`.

Aspect Pure_Function
====================
.. index:: Pure_Function

This boolean aspect is equivalent to pragma `Pure_Function`.

Aspect Refined_Depends
======================
.. index:: Refined_Depends

This aspect is equivalent to pragma `Refined_Depends`.

Aspect Refined_Global
=====================
.. index:: Refined_Global

This aspect is equivalent to pragma `Refined_Global`.

Aspect Refined_Post
===================
.. index:: Refined_Post

This aspect is equivalent to pragma `Refined_Post`.

Aspect Refined_State
====================
.. index:: Refined_State

This aspect is equivalent to pragma `Refined_State`.

Aspect Remote_Access_Type
=========================
.. index:: Remote_Access_Type

This aspect is equivalent to pragma `Remote_Access_Type`.

Aspect Scalar_Storage_Order
===========================
.. index:: Scalar_Storage_Order

This aspect is equivalent to a `Scalar_Storage_Order`
attribute definition clause.

Aspect Shared
=============
.. index:: Shared

This boolean aspect is equivalent to pragma `Shared`,
and is thus a synonym for aspect `Atomic`.

Aspect Simple_Storage_Pool
==========================
.. index:: Simple_Storage_Pool

This aspect is equivalent to a `Simple_Storage_Pool`
attribute definition clause.

Aspect Simple_Storage_Pool_Type
===============================
.. index:: Simple_Storage_Pool_Type

This boolean aspect is equivalent to pragma `Simple_Storage_Pool_Type`.

Aspect SPARK_Mode
=================
.. index:: SPARK_Mode

This aspect is equivalent to pragma `SPARK_Mode` and
may be specified for either or both of the specification and body
of a subprogram or package.

Aspect Suppress_Debug_Info
==========================
.. index:: Suppress_Debug_Info

This boolean aspect is equivalent to pragma `Suppress_Debug_Info`.

Aspect Suppress_Initialization
==============================
.. index:: Suppress_Initialization

This boolean aspect is equivalent to pragma `Suppress_Initialization`.

Aspect Test_Case
================
.. index:: Test_Case

This aspect is equivalent to pragma `Test_Case`.

Aspect Thread_Local_Storage
===========================
.. index:: Thread_Local_Storage

This boolean aspect is equivalent to pragma `Thread_Local_Storage`.

Aspect Universal_Aliasing
=========================
.. index:: Universal_Aliasing

This boolean aspect is equivalent to pragma `Universal_Aliasing`.

Aspect Universal_Data
=====================
.. index:: Universal_Data

This aspect is equivalent to pragma `Universal_Data`.

Aspect Unmodified
=================
.. index:: Unmodified

This boolean aspect is equivalent to pragma `Unmodified`.

Aspect Unreferenced
===================
.. index:: Unreferenced

This boolean aspect is equivalent to pragma `Unreferenced`. Note that
in the case of formal parameters, it is not permitted to have aspects for
a formal parameter, so in this case the pragma form must be used.

Aspect Unreferenced_Objects
===========================
.. index:: Unreferenced_Objects

This boolean aspect is equivalent to pragma `Unreferenced_Objects`.

Aspect Value_Size
=================
.. index:: Value_Size

This aspect is equivalent to a `Value_Size`
attribute definition clause.

Aspect Warnings
===============
.. index:: Warnings

This aspect is equivalent to the two argument form of pragma `Warnings`,
where the first argument is `ON` or `OFF` and the second argument
is the entity.
