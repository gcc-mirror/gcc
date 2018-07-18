.. _Compatibility_and_Porting_Guide:

*******************************
Compatibility and Porting Guide
*******************************

This chapter presents some guidelines for developing portable Ada code,
describes the compatibility issues that may arise between
GNAT and other Ada compilation systems (including those for Ada 83),
and shows how GNAT can expedite porting
applications developed in other Ada environments.

.. _Writing_Portable_Fixed-Point_Declarations:

Writing Portable Fixed-Point Declarations
=========================================

The Ada Reference Manual gives an implementation freedom to choose bounds
that are narrower by ``Small`` from the given bounds.
For example, if we write

.. code-block:: ada

     type F1 is delta 1.0 range -128.0 .. +128.0;

then the implementation is allowed to choose -128.0 .. +127.0 if it
likes, but is not required to do so.

This leads to possible portability problems, so let's have a closer
look at this, and figure out how to avoid these problems.

First, why does this freedom exist, and why would an implementation
take advantage of it? To answer this, take a closer look at the type
declaration for ``F1`` above. If the compiler uses the given bounds,
it would need 9 bits to hold the largest positive value (and typically
that means 16 bits on all machines). But if the implementation chooses
the +127.0 bound then it can fit values of the type in 8 bits.

Why not make the user write +127.0 if that's what is wanted?
The rationale is that if you are thinking of fixed point
as a kind of 'poor man's floating-point', then you don't want
to be thinking about the scaled integers that are used in its
representation. Let's take another example:

.. code-block:: ada

      type F2 is delta 2.0**(-15) range -1.0 .. +1.0;

Looking at this declaration, it seems casually as though
it should fit in 16 bits, but again that extra positive value
+1.0 has the scaled integer equivalent of 2**15 which is one too
big for signed 16 bits. The implementation can treat this as:

.. code-block:: ada

     type F2 is delta 2.0**(-15) range -1.0 .. +1.0-(2.0**(-15));

and the Ada language design team felt that this was too annoying
to require. We don't need to debate this decision at this point,
since it is well established (the rule about narrowing the ranges
dates to Ada 83).

But the important point is that an implementation is not required
to do this narrowing, so we have a potential portability problem.
We could imagine three types of implementation:

(a) those that narrow the range automatically if they can figure
    out that the narrower range will allow storage in a smaller machine unit,

(b) those that will narrow only if forced to by a ``'Size`` clause, and

(c) those that will never narrow.

Now if we are language theoreticians, we can imagine a fourth
approach: to narrow all the time, e.g. to treat

.. code-block:: ada

     type F3 is delta 1.0 range -10.0 .. +23.0;

as though it had been written:


.. code-block:: ada

      type F3 is delta 1.0 range -9.0 .. +22.0;

But although technically allowed, such a behavior would be hostile and silly,
and no real compiler would do this. All real compilers will fall into one of
the categories (a), (b) or (c) above.

So, how do you get the compiler to do what you want? The answer is give the
actual bounds you want, and then use a ``'Small`` clause and a
``'Size`` clause to absolutely pin down what the compiler does.
E.g., for ``F2`` above, we will write:

.. code-block:: ada

     My_Small : constant := 2.0**(-15);
     My_First : constant := -1.0;
     My_Last  : constant := +1.0 - My_Small;

     type F2 is delta My_Small range My_First .. My_Last;

and then add

.. code-block:: ada

     for F2'Small use my_Small;
     for F2'Size  use 16;

In practice all compilers will do the same thing here and will give you
what you want, so the above declarations are fully portable. If you really
want to play language lawyer and guard against ludicrous behavior by the
compiler you could add

.. code-block:: ada

     Test1 : constant := 1 / Boolean'Pos (F2'First = My_First);
     Test2 : constant := 1 / Boolean'Pos (F2'Last  = My_Last);

One or other or both are allowed to be illegal if the compiler is
behaving in a silly manner, but at least the silly compiler will not
get away with silently messing with your (very clear) intentions.

If you follow this scheme you will be guaranteed that your fixed-point
types will be portable.




.. _Compatibility_with_Ada_83:

Compatibility with Ada 83
=========================

.. index:: Compatibility (between Ada 83 and Ada 95 / Ada 2005 / Ada 2012)

Ada 95 and the subsequent revisions Ada 2005 and Ada 2012
are highly upwards compatible with Ada 83.  In
particular, the design intention was that the difficulties associated
with moving from Ada 83 to later versions of the standard should be no greater
than those that occur when moving from one Ada 83 system to another.

However, there are a number of points at which there are minor
incompatibilities.  The :title:`Ada 95 Annotated Reference Manual` contains
full details of these issues as they relate to Ada 95,
and should be consulted for a complete treatment.
In practice the
following subsections treat the most likely issues to be encountered.

.. _Legal_Ada_83_programs_that_are_illegal_in_Ada_95:

Legal Ada 83 programs that are illegal in Ada 95
------------------------------------------------

Some legal Ada 83 programs are illegal (i.e., they will fail to compile) in
Ada 95 and later versions of the standard:


* *Character literals*

  Some uses of character literals are ambiguous.  Since Ada 95 has introduced
  ``Wide_Character`` as a new predefined character type, some uses of
  character literals that were legal in Ada 83 are illegal in Ada 95.
  For example:

  .. code-block:: ada

       for Char in 'A' .. 'Z' loop ... end loop;

  The problem is that 'A' and 'Z' could be from either
  ``Character`` or ``Wide_Character``.  The simplest correction
  is to make the type explicit; e.g.:

  .. code-block:: ada

       for Char in Character range 'A' .. 'Z' loop ... end loop;

* *New reserved words*

  The identifiers ``abstract``, ``aliased``, ``protected``,
  ``requeue``, ``tagged``, and ``until`` are reserved in Ada 95.
  Existing Ada 83 code using any of these identifiers must be edited to
  use some alternative name.

* *Freezing rules*

  The rules in Ada 95 are slightly different with regard to the point at
  which entities are frozen, and representation pragmas and clauses are
  not permitted past the freeze point.  This shows up most typically in
  the form of an error message complaining that a representation item
  appears too late, and the appropriate corrective action is to move
  the item nearer to the declaration of the entity to which it refers.

  A particular case is that representation pragmas
  cannot be applied to a subprogram body.  If necessary, a separate subprogram
  declaration must be introduced to which the pragma can be applied.

* *Optional bodies for library packages*

  In Ada 83, a package that did not require a package body was nevertheless
  allowed to have one.  This lead to certain surprises in compiling large
  systems (situations in which the body could be unexpectedly ignored by the
  binder).  In Ada 95, if a package does not require a body then it is not
  permitted to have a body.  To fix this problem, simply remove a redundant
  body if it is empty, or, if it is non-empty, introduce a dummy declaration
  into the spec that makes the body required.  One approach is to add a private
  part to the package declaration (if necessary), and define a parameterless
  procedure called ``Requires_Body``, which must then be given a dummy
  procedure body in the package body, which then becomes required.
  Another approach (assuming that this does not introduce elaboration
  circularities) is to add an ``Elaborate_Body`` pragma to the package spec,
  since one effect of this pragma is to require the presence of a package body.

* *Numeric_Error is the same exception as Constraint_Error*

  In Ada 95, the exception ``Numeric_Error`` is a renaming of ``Constraint_Error``.
  This means that it is illegal to have separate exception handlers for
  the two exceptions.  The fix is simply to remove the handler for the
  ``Numeric_Error`` case (since even in Ada 83, a compiler was free to raise
  ``Constraint_Error`` in place of ``Numeric_Error`` in all cases).

* *Indefinite subtypes in generics*

  In Ada 83, it was permissible to pass an indefinite type (e.g, ``String``)
  as the actual for a generic formal private type, but then the instantiation
  would be illegal if there were any instances of declarations of variables
  of this type in the generic body.  In Ada 95, to avoid this clear violation
  of the methodological principle known as the 'contract model',
  the generic declaration explicitly indicates whether
  or not such instantiations are permitted.  If a generic formal parameter
  has explicit unknown discriminants, indicated by using ``(<>)`` after the
  subtype name, then it can be instantiated with indefinite types, but no
  stand-alone variables can be declared of this type.  Any attempt to declare
  such a variable will result in an illegality at the time the generic is
  declared.  If the ``(<>)`` notation is not used, then it is illegal
  to instantiate the generic with an indefinite type.
  This is the potential incompatibility issue when porting Ada 83 code to Ada 95.
  It will show up as a compile time error, and
  the fix is usually simply to add the ``(<>)`` to the generic declaration.


.. _More_deterministic_semantics:

More deterministic semantics
----------------------------

* *Conversions*

  Conversions from real types to integer types round away from 0.  In Ada 83
  the conversion Integer(2.5) could deliver either 2 or 3 as its value.  This
  implementation freedom was intended to support unbiased rounding in
  statistical applications, but in practice it interfered with portability.
  In Ada 95 the conversion semantics are unambiguous, and rounding away from 0
  is required.  Numeric code may be affected by this change in semantics.
  Note, though, that this issue is no worse than already existed in Ada 83
  when porting code from one vendor to another.

* *Tasking*

  The Real-Time Annex introduces a set of policies that define the behavior of
  features that were implementation dependent in Ada 83, such as the order in
  which open select branches are executed.


.. _Changed_semantics:

Changed semantics
-----------------

The worst kind of incompatibility is one where a program that is legal in
Ada 83 is also legal in Ada 95 but can have an effect in Ada 95 that was not
possible in Ada 83.  Fortunately this is extremely rare, but the one
situation that you should be alert to is the change in the predefined type
``Character`` from 7-bit ASCII to 8-bit Latin-1.

    .. index:: Latin-1

* *Range of type ``Character``*

  The range of ``Standard.Character`` is now the full 256 characters
  of Latin-1, whereas in most Ada 83 implementations it was restricted
  to 128 characters. Although some of the effects of
  this change will be manifest in compile-time rejection of legal
  Ada 83 programs it is possible for a working Ada 83 program to have
  a different effect in Ada 95, one that was not permitted in Ada 83.
  As an example, the expression
  ``Character'Pos(Character'Last)`` returned ``127`` in Ada 83 and now
  delivers ``255`` as its value.
  In general, you should look at the logic of any
  character-processing Ada 83 program and see whether it needs to be adapted
  to work correctly with Latin-1.  Note that the predefined Ada 95 API has a
  character handling package that may be relevant if code needs to be adapted
  to account for the additional Latin-1 elements.
  The desirable fix is to
  modify the program to accommodate the full character set, but in some cases
  it may be convenient to define a subtype or derived type of Character that
  covers only the restricted range.


.. _Other_language_compatibility_issues:

Other language compatibility issues
-----------------------------------

* *-gnat83* switch

  All implementations of GNAT provide a switch that causes GNAT to operate
  in Ada 83 mode.  In this mode, some but not all compatibility problems
  of the type described above are handled automatically.  For example, the
  new reserved words introduced in Ada 95 and Ada 2005 are treated simply
  as identifiers as in Ada 83.  However,
  in practice, it is usually advisable to make the necessary modifications
  to the program to remove the need for using this switch.
  See the ``Compiling Different Versions of Ada`` section in
  the :title:`GNAT User's Guide`.


* Support for removed Ada 83 pragmas and attributes

  A number of pragmas and attributes from Ada 83 were removed from Ada 95,
  generally because they were replaced by other mechanisms.  Ada 95 and Ada 2005
  compilers are allowed, but not required, to implement these missing
  elements.  In contrast with some other compilers, GNAT implements all
  such pragmas and attributes, eliminating this compatibility concern.  These
  include ``pragma Interface`` and the floating point type attributes
  (``Emax``, ``Mantissa``, etc.), among other items.


.. _Compatibility_between_Ada_95_and_Ada_2005:

Compatibility between Ada 95 and Ada 2005
=========================================

.. index:: Compatibility between Ada 95 and Ada 2005

Although Ada 2005 was designed to be upwards compatible with Ada 95, there are
a number of incompatibilities. Several are enumerated below;
for a complete description please see the
:title:`Annotated Ada 2005 Reference Manual`, or section 9.1.1 in
:title:`Rationale for Ada 2005`.

* *New reserved words.*

  The words ``interface``, ``overriding`` and ``synchronized`` are
  reserved in Ada 2005.
  A pre-Ada 2005 program that uses any of these as an identifier will be
  illegal.

* *New declarations in predefined packages.*

  A number of packages in the predefined environment contain new declarations:
  ``Ada.Exceptions``, ``Ada.Real_Time``, ``Ada.Strings``,
  ``Ada.Strings.Fixed``, ``Ada.Strings.Bounded``,
  ``Ada.Strings.Unbounded``, ``Ada.Strings.Wide_Fixed``,
  ``Ada.Strings.Wide_Bounded``, ``Ada.Strings.Wide_Unbounded``,
  ``Ada.Tags``, ``Ada.Text_IO``, and ``Interfaces.C``.
  If an Ada 95 program does a ``with`` and ``use`` of any of these
  packages, the new declarations may cause name clashes.

* *Access parameters.*

  A nondispatching subprogram with an access parameter cannot be renamed
  as a dispatching operation.  This was permitted in Ada 95.

* *Access types, discriminants, and constraints.*

  Rule changes in this area have led to some incompatibilities; for example,
  constrained subtypes of some access types are not permitted in Ada 2005.

* *Aggregates for limited types.*

  The allowance of aggregates for limited types in Ada 2005 raises the
  possibility of ambiguities in legal Ada 95 programs, since additional types
  now need to be considered in expression resolution.

* *Fixed-point multiplication and division.*

  Certain expressions involving '*' or '/' for a fixed-point type, which
  were legal in Ada 95 and invoked the predefined versions of these operations,
  are now ambiguous.
  The ambiguity may be resolved either by applying a type conversion to the
  expression, or by explicitly invoking the operation from package
  ``Standard``.

* *Return-by-reference types.*

  The Ada 95 return-by-reference mechanism has been removed.  Instead, the user
  can declare a function returning a value from an anonymous access type.


.. _Implementation-dependent_characteristics:

Implementation-dependent characteristics
========================================

Although the Ada language defines the semantics of each construct as
precisely as practical, in some situations (for example for reasons of
efficiency, or where the effect is heavily dependent on the host or target
platform) the implementation is allowed some freedom.  In porting Ada 83
code to GNAT, you need to be aware of whether / how the existing code
exercised such implementation dependencies.  Such characteristics fall into
several categories, and GNAT offers specific support in assisting the
transition from certain Ada 83 compilers.

.. _Implementation-defined_pragmas:

Implementation-defined pragmas
------------------------------

Ada compilers are allowed to supplement the language-defined pragmas, and
these are a potential source of non-portability.  All GNAT-defined pragmas
are described in :ref:`Implementation_Defined_Pragmas`,
and these include several that are specifically
intended to correspond to other vendors' Ada 83 pragmas.
For migrating from VADS, the pragma ``Use_VADS_Size`` may be useful.
For compatibility with HP Ada 83, GNAT supplies the pragmas
``Extend_System``, ``Ident``, ``Inline_Generic``,
``Interface_Name``, ``Passive``, ``Suppress_All``,
and ``Volatile``.
Other relevant pragmas include ``External`` and ``Link_With``.
Some vendor-specific
Ada 83 pragmas (``Share_Generic``, ``Subtitle``, and ``Title``) are
recognized, thus
avoiding compiler rejection of units that contain such pragmas; they are not
relevant in a GNAT context and hence are not otherwise implemented.


.. _Implementation-defined_attributes:

Implementation-defined attributes
---------------------------------

Analogous to pragmas, the set of attributes may be extended by an
implementation.  All GNAT-defined attributes are described in
:ref:`Implementation_Defined_Attributes`,
and these include several that are specifically intended
to correspond to other vendors' Ada 83 attributes.  For migrating from VADS,
the attribute ``VADS_Size`` may be useful.  For compatibility with HP
Ada 83, GNAT supplies the attributes ``Bit``, ``Machine_Size`` and
``Type_Class``.

.. _Libraries:

Libraries
---------

Vendors may supply libraries to supplement the standard Ada API.  If Ada 83
code uses vendor-specific libraries then there are several ways to manage
this in Ada 95 and later versions of the standard:

* If the source code for the libraries (specs and bodies) are
  available, then the libraries can be migrated in the same way as the
  application.

* If the source code for the specs but not the bodies are
  available, then you can reimplement the bodies.

* Some features introduced by Ada 95 obviate the need for library support.  For
  example most Ada 83 vendors supplied a package for unsigned integers.  The
  Ada 95 modular type feature is the preferred way to handle this need, so
  instead of migrating or reimplementing the unsigned integer package it may
  be preferable to retrofit the application using modular types.


.. _Elaboration_order:

Elaboration order
-----------------
The implementation can choose any elaboration order consistent with the unit
dependency relationship.  This freedom means that some orders can result in
Program_Error being raised due to an 'Access Before Elaboration': an attempt
to invoke a subprogram before its body has been elaborated, or to instantiate
a generic before the generic body has been elaborated.  By default GNAT
attempts to choose a safe order (one that will not encounter access before
elaboration problems) by implicitly inserting ``Elaborate`` or
``Elaborate_All`` pragmas where
needed.  However, this can lead to the creation of elaboration circularities
and a resulting rejection of the program by gnatbind.  This issue is
thoroughly described in the *Elaboration Order Handling in GNAT* appendix
in the :title:`GNAT User's Guide`.
In brief, there are several
ways to deal with this situation:

* Modify the program to eliminate the circularities, e.g., by moving
  elaboration-time code into explicitly-invoked procedures

* Constrain the elaboration order by including explicit ``Elaborate_Body`` or
  ``Elaborate`` pragmas, and then inhibit the generation of implicit
  ``Elaborate_All``
  pragmas either globally (as an effect of the *-gnatE* switch) or locally
  (by selectively suppressing elaboration checks via pragma
  ``Suppress(Elaboration_Check)`` when it is safe to do so).


.. _Target-specific_aspects:

Target-specific aspects
-----------------------

Low-level applications need to deal with machine addresses, data
representations, interfacing with assembler code, and similar issues.  If
such an Ada 83 application is being ported to different target hardware (for
example where the byte endianness has changed) then you will need to
carefully examine the program logic; the porting effort will heavily depend
on the robustness of the original design.  Moreover, Ada 95 (and thus
Ada 2005 and Ada 2012) are sometimes
incompatible with typical Ada 83 compiler practices regarding implicit
packing, the meaning of the Size attribute, and the size of access values.
GNAT's approach to these issues is described in :ref:`Representation_Clauses`.


.. _Compatibility_with_Other_Ada_Systems:

Compatibility with Other Ada Systems
====================================

If programs avoid the use of implementation dependent and
implementation defined features, as documented in the
:title:`Ada Reference Manual`, there should be a high degree of portability between
GNAT and other Ada systems.  The following are specific items which
have proved troublesome in moving Ada 95 programs from GNAT to other Ada 95
compilers, but do not affect porting code to GNAT.
(As of January 2007, GNAT is the only compiler available for Ada 2005;
the following issues may or may not arise for Ada 2005 programs
when other compilers appear.)

* *Ada 83 Pragmas and Attributes*

  Ada 95 compilers are allowed, but not required, to implement the missing
  Ada 83 pragmas and attributes that are no longer defined in Ada 95.
  GNAT implements all such pragmas and attributes, eliminating this as
  a compatibility concern, but some other Ada 95 compilers reject these
  pragmas and attributes.

* *Specialized Needs Annexes*

  GNAT implements the full set of special needs annexes.  At the
  current time, it is the only Ada 95 compiler to do so.  This means that
  programs making use of these features may not be portable to other Ada
  95 compilation systems.

* *Representation Clauses*

  Some other Ada 95 compilers implement only the minimal set of
  representation clauses required by the Ada 95 reference manual.  GNAT goes
  far beyond this minimal set, as described in the next section.


.. _Representation_Clauses:

Representation Clauses
======================

The Ada 83 reference manual was quite vague in describing both the minimal
required implementation of representation clauses, and also their precise
effects.  Ada 95 (and thus also Ada 2005) are much more explicit, but the
minimal set of capabilities required is still quite limited.

GNAT implements the full required set of capabilities in
Ada 95 and Ada 2005, but also goes much further, and in particular
an effort has been made to be compatible with existing Ada 83 usage to the
greatest extent possible.

A few cases exist in which Ada 83 compiler behavior is incompatible with
the requirements in Ada 95 (and thus also Ada 2005).  These are instances of
intentional or accidental dependence on specific implementation dependent
characteristics of these Ada 83 compilers.  The following is a list of
the cases most likely to arise in existing Ada 83 code.

* *Implicit Packing*

  Some Ada 83 compilers allowed a Size specification to cause implicit
  packing of an array or record.  This could cause expensive implicit
  conversions for change of representation in the presence of derived
  types, and the Ada design intends to avoid this possibility.
  Subsequent AI's were issued to make it clear that such implicit
  change of representation in response to a Size clause is inadvisable,
  and this recommendation is represented explicitly in the Ada 95 (and Ada 2005)
  Reference Manuals as implementation advice that is followed by GNAT.
  The problem will show up as an error
  message rejecting the size clause.  The fix is simply to provide
  the explicit pragma ``Pack``, or for more fine tuned control, provide
  a Component_Size clause.

* *Meaning of Size Attribute*

  The Size attribute in Ada 95 (and Ada 2005) for discrete types is defined as
  the minimal number of bits required to hold values of the type.  For example,
  on a 32-bit machine, the size of ``Natural`` will typically be 31 and not
  32 (since no sign bit is required).  Some Ada 83 compilers gave 31, and
  some 32 in this situation.  This problem will usually show up as a compile
  time error, but not always.  It is a good idea to check all uses of the
  'Size attribute when porting Ada 83 code.  The GNAT specific attribute
  Object_Size can provide a useful way of duplicating the behavior of
  some Ada 83 compiler systems.

* *Size of Access Types*

  A common assumption in Ada 83 code is that an access type is in fact a pointer,
  and that therefore it will be the same size as a System.Address value.  This
  assumption is true for GNAT in most cases with one exception.  For the case of
  a pointer to an unconstrained array type (where the bounds may vary from one
  value of the access type to another), the default is to use a 'fat pointer',
  which is represented as two separate pointers, one to the bounds, and one to
  the array.  This representation has a number of advantages, including improved
  efficiency.  However, it may cause some difficulties in porting existing Ada 83
  code which makes the assumption that, for example, pointers fit in 32 bits on
  a machine with 32-bit addressing.

  To get around this problem, GNAT also permits the use of 'thin pointers' for
  access types in this case (where the designated type is an unconstrained array
  type).  These thin pointers are indeed the same size as a System.Address value.
  To specify a thin pointer, use a size clause for the type, for example:

  .. code-block:: ada

       type X is access all String;
       for X'Size use Standard'Address_Size;

  which will cause the type X to be represented using a single pointer.
  When using this representation, the bounds are right behind the array.
  This representation is slightly less efficient, and does not allow quite
  such flexibility in the use of foreign pointers or in using the
  Unrestricted_Access attribute to create pointers to non-aliased objects.
  But for any standard portable use of the access type it will work in
  a functionally correct manner and allow porting of existing code.
  Note that another way of forcing a thin pointer representation
  is to use a component size clause for the element size in an array,
  or a record representation clause for an access field in a record.

  See the documentation of Unrestricted_Access in the GNAT RM for a
  full discussion of possible problems using this attribute in conjunction
  with thin pointers.


.. _Compatibility_with_HP_Ada_83:

Compatibility with HP Ada 83
============================

All the HP Ada 83 pragmas and attributes are recognized, although only a subset
of them can sensibly be implemented.  The description of pragmas in
:ref:`Implementation_Defined_Pragmas` indicates whether or not they are
applicable to GNAT.

* *Default floating-point representation*

  In GNAT, the default floating-point format is IEEE, whereas in HP Ada 83,
  it is VMS format.

* *System*

  the package System in GNAT exactly corresponds to the definition in the
  Ada 95 reference manual, which means that it excludes many of the
  HP Ada 83 extensions.  However, a separate package Aux_DEC is provided
  that contains the additional definitions, and a special pragma,
  Extend_System allows this package to be treated transparently as an
  extension of package System.
