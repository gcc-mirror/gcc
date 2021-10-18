.. role:: switch(samp)

.. _Implementation_Defined_Pragmas:

******************************
Implementation Defined Pragmas
******************************

Ada defines a set of pragmas that can be used to supply additional
information to the compiler.  These language defined pragmas are
implemented in GNAT and work as described in the Ada Reference Manual.

In addition, Ada allows implementations to define additional pragmas
whose meaning is defined by the implementation.  GNAT provides a number
of these implementation-defined pragmas, which can be used to extend
and enhance the functionality of the compiler.  This section of the GNAT
Reference Manual describes these additional pragmas.

Note that any program using these pragmas might not be portable to other
compilers (although GNAT implements this set of pragmas on all
platforms).  Therefore if portability to other compilers is an important
consideration, the use of these pragmas should be minimized.

Pragma Abort_Defer
==================

.. index:: Deferring aborts

Syntax:

.. code-block:: ada

  pragma Abort_Defer;


This pragma must appear at the start of the statement sequence of a
handled sequence of statements (right after the ``begin``).  It has
the effect of deferring aborts for the sequence of statements (but not
for the declarations or handlers, if any, associated with this statement
sequence). This can also be useful for adding a polling point in Ada code,
where asynchronous abort of tasks is checked when leaving the statement
sequence, and is lighter than, for example, using ``delay 0.0;``, since with
zero-cost exception handling, propagating exceptions (implicitly used to
implement task abort) cannot be done reliably in an asynchronous way.

An example of usage would be:

.. code-block:: ada

  --  Add a polling point to check for task aborts

  begin
     pragma Abort_Defer;
  end;

.. _Pragma-Abstract_State:

Pragma Abstract_State
=====================

Syntax:

.. code-block:: ada

  pragma Abstract_State (ABSTRACT_STATE_LIST);

  ABSTRACT_STATE_LIST ::=
       null
    |  STATE_NAME_WITH_OPTIONS
    | (STATE_NAME_WITH_OPTIONS {, STATE_NAME_WITH_OPTIONS} )

  STATE_NAME_WITH_OPTIONS ::=
       STATE_NAME
    | (STATE_NAME with OPTION_LIST)

  OPTION_LIST ::= OPTION {, OPTION}

  OPTION ::=
      SIMPLE_OPTION
    | NAME_VALUE_OPTION

  SIMPLE_OPTION ::= Ghost | Synchronous

  NAME_VALUE_OPTION ::=
      Part_Of => ABSTRACT_STATE
    | External [=> EXTERNAL_PROPERTY_LIST]

  EXTERNAL_PROPERTY_LIST ::=
       EXTERNAL_PROPERTY
    | (EXTERNAL_PROPERTY {, EXTERNAL_PROPERTY} )

  EXTERNAL_PROPERTY ::=
      Async_Readers    [=> boolean_EXPRESSION]
    | Async_Writers    [=> boolean_EXPRESSION]
    | Effective_Reads  [=> boolean_EXPRESSION]
    | Effective_Writes [=> boolean_EXPRESSION]
      others            => boolean_EXPRESSION

  STATE_NAME ::= defining_identifier

  ABSTRACT_STATE ::= name

For the semantics of this pragma, see the entry for aspect ``Abstract_State`` in
the SPARK 2014 Reference Manual, section 7.1.4.

Pragma Ada_83
=============

Syntax:

.. code-block:: ada

  pragma Ada_83;


A configuration pragma that establishes Ada 83 mode for the unit to
which it applies, regardless of the mode set by the command line
switches.  In Ada 83 mode, GNAT attempts to be as compatible with
the syntax and semantics of Ada 83, as defined in the original Ada
83 Reference Manual as possible.  In particular, the keywords added by Ada 95
and Ada 2005 are not recognized, optional package bodies are allowed,
and generics may name types with unknown discriminants without using
the ``(<>)`` notation.  In addition, some but not all of the additional
restrictions of Ada 83 are enforced.

Ada 83 mode is intended for two purposes.  Firstly, it allows existing
Ada 83 code to be compiled and adapted to GNAT with less effort.
Secondly, it aids in keeping code backwards compatible with Ada 83.
However, there is no guarantee that code that is processed correctly
by GNAT in Ada 83 mode will in fact compile and execute with an Ada
83 compiler, since GNAT does not enforce all the additional checks
required by Ada 83.

Pragma Ada_95
=============

Syntax:

.. code-block:: ada

  pragma Ada_95;


A configuration pragma that establishes Ada 95 mode for the unit to which
it applies, regardless of the mode set by the command line switches.
This mode is set automatically for the ``Ada`` and ``System``
packages and their children, so you need not specify it in these
contexts.  This pragma is useful when writing a reusable component that
itself uses Ada 95 features, but which is intended to be usable from
either Ada 83 or Ada 95 programs.

Pragma Ada_05
=============

Syntax:

.. code-block:: ada

  pragma Ada_05;
  pragma Ada_05 (local_NAME);


A configuration pragma that establishes Ada 2005 mode for the unit to which
it applies, regardless of the mode set by the command line switches.
This pragma is useful when writing a reusable component that
itself uses Ada 2005 features, but which is intended to be usable from
either Ada 83 or Ada 95 programs.

The one argument form (which is not a configuration pragma)
is used for managing the transition from
Ada 95 to Ada 2005 in the run-time library. If an entity is marked
as Ada_2005 only, then referencing the entity in Ada_83 or Ada_95
mode will generate a warning. In addition, in Ada_83 or Ada_95
mode, a preference rule is established which does not choose
such an entity unless it is unambiguously specified. This avoids
extra subprograms marked this way from generating ambiguities in
otherwise legal pre-Ada_2005 programs. The one argument form is
intended for exclusive use in the GNAT run-time library.

Pragma Ada_2005
===============

Syntax:

.. code-block:: ada

  pragma Ada_2005;


This configuration pragma is a synonym for pragma Ada_05 and has the
same syntax and effect.

Pragma Ada_12
=============

Syntax:

.. code-block:: ada

  pragma Ada_12;
  pragma Ada_12 (local_NAME);


A configuration pragma that establishes Ada 2012 mode for the unit to which
it applies, regardless of the mode set by the command line switches.
This mode is set automatically for the ``Ada`` and ``System``
packages and their children, so you need not specify it in these
contexts.  This pragma is useful when writing a reusable component that
itself uses Ada 2012 features, but which is intended to be usable from
Ada 83, Ada 95, or Ada 2005 programs.

The one argument form, which is not a configuration pragma,
is used for managing the transition from Ada
2005 to Ada 2012 in the run-time library. If an entity is marked
as Ada_2012 only, then referencing the entity in any pre-Ada_2012
mode will generate a warning. In addition, in any pre-Ada_2012
mode, a preference rule is established which does not choose
such an entity unless it is unambiguously specified. This avoids
extra subprograms marked this way from generating ambiguities in
otherwise legal pre-Ada_2012 programs. The one argument form is
intended for exclusive use in the GNAT run-time library.

Pragma Ada_2012
===============

Syntax:

.. code-block:: ada

  pragma Ada_2012;


This configuration pragma is a synonym for pragma Ada_12 and has the
same syntax and effect.

Pragma Aggregate_Individually_Assign
====================================

Syntax:

.. code-block:: ada

  pragma Aggregate_Individually_Assign;

Where possible, GNAT will store the binary representation of a record aggregate
in memory for space and performance reasons. This configuration pragma changes
this behavior so that record aggregates are instead always converted into
individual assignment statements.


Pragma Allow_Integer_Address
============================

Syntax:

.. code-block:: ada

  pragma Allow_Integer_Address;


In almost all versions of GNAT, ``System.Address`` is a private
type in accordance with the implementation advice in the RM. This
means that integer values,
in particular integer literals, are not allowed as address values.
If the configuration pragma
``Allow_Integer_Address`` is given, then integer expressions may
be used anywhere a value of type ``System.Address`` is required.
The effect is to introduce an implicit unchecked conversion from the
integer value to type ``System.Address``. The reverse case of using
an address where an integer type is required is handled analogously.
The following example compiles without errors:


.. code-block:: ada

  pragma Allow_Integer_Address;
  with System; use System;
  package AddrAsInt is
     X : Integer;
     Y : Integer;
     for X'Address use 16#1240#;
     for Y use at 16#3230#;
     m : Address := 16#4000#;
     n : constant Address := 4000;
     p : constant Address := Address (X + Y);
     v : Integer := y'Address;
     w : constant Integer := Integer (Y'Address);
     type R is new integer;
     RR : R := 1000;
     Z : Integer;
     for Z'Address use RR;
  end AddrAsInt;


Note that pragma ``Allow_Integer_Address`` is ignored if ``System.Address``
is not a private type. In implementations of ``GNAT`` where
System.Address is a visible integer type,
this pragma serves no purpose but is ignored
rather than rejected to allow common sets of sources to be used
in the two situations.

.. _Pragma-Annotate:

Pragma Annotate
===============

Syntax::

  pragma Annotate (IDENTIFIER [, IDENTIFIER {, ARG}] [, entity => local_NAME]);

  ARG ::= NAME | EXPRESSION


This pragma is used to annotate programs.  IDENTIFIER identifies
the type of annotation.  GNAT verifies that it is an identifier, but does
not otherwise analyze it. The second optional identifier is also left
unanalyzed, and by convention is used to control the action of the tool to
which the annotation is addressed.  The remaining ARG arguments
can be either string literals or more generally expressions.
String literals (and concatenations of string literals) are assumed to be
either of type
``Standard.String`` or else ``Wide_String`` or ``Wide_Wide_String``
depending on the character literals they contain.
All other kinds of arguments are analyzed as expressions, and must be
unambiguous. The last argument if present must have the identifier
``Entity`` and GNAT verifies that a local name is given.

The analyzed pragma is retained in the tree, but not otherwise processed
by any part of the GNAT compiler, except to generate corresponding note
lines in the generated ALI file. For the format of these note lines, see
the compiler source file lib-writ.ads. This pragma is intended for use by
external tools, including ASIS. The use of pragma Annotate does not
affect the compilation process in any way. This pragma may be used as
a configuration pragma.

Pragma Assert
=============

Syntax::

  pragma Assert (
    boolean_EXPRESSION
    [, string_EXPRESSION]);


The effect of this pragma depends on whether the corresponding command
line switch is set to activate assertions.  The pragma expands into code
equivalent to the following:

.. code-block:: ada

  if assertions-enabled then
     if not boolean_EXPRESSION then
        System.Assertions.Raise_Assert_Failure
          (string_EXPRESSION);
     end if;
  end if;


The string argument, if given, is the message that will be associated
with the exception occurrence if the exception is raised.  If no second
argument is given, the default message is ``file``:``nnn``,
where ``file`` is the name of the source file containing the assert,
and ``nnn`` is the line number of the assert.

Note that, as with the ``if`` statement to which it is equivalent, the
type of the expression is either ``Standard.Boolean``, or any type derived
from this standard type.

Assert checks can be either checked or ignored. By default they are ignored.
They will be checked if either the command line switch *-gnata* is
used, or if an ``Assertion_Policy`` or ``Check_Policy`` pragma is used
to enable ``Assert_Checks``.

If assertions are ignored, then there
is no run-time effect (and in particular, any side effects from the
expression will not occur at run time).  (The expression is still
analyzed at compile time, and may cause types to be frozen if they are
mentioned here for the first time).

If assertions are checked, then the given expression is tested, and if
it is ``False`` then ``System.Assertions.Raise_Assert_Failure`` is called
which results in the raising of ``Assert_Failure`` with the given message.

You should generally avoid side effects in the expression arguments of
this pragma, because these side effects will turn on and off with the
setting of the assertions mode, resulting in assertions that have an
effect on the program.  However, the expressions are analyzed for
semantic correctness whether or not assertions are enabled, so turning
assertions on and off cannot affect the legality of a program.

Note that the implementation defined policy ``DISABLE``, given in a
pragma ``Assertion_Policy``, can be used to suppress this semantic analysis.

Note: this is a standard language-defined pragma in versions
of Ada from 2005 on. In GNAT, it is implemented in all versions
of Ada, and the DISABLE policy is an implementation-defined
addition.

Pragma Assert_And_Cut
=====================

Syntax::

  pragma Assert_And_Cut (
    boolean_EXPRESSION
    [, string_EXPRESSION]);


The effect of this pragma is identical to that of pragma ``Assert``,
except that in an ``Assertion_Policy`` pragma, the identifier
``Assert_And_Cut`` is used to control whether it is ignored or checked
(or disabled).

The intention is that this be used within a subprogram when the
given test expresion sums up all the work done so far in the
subprogram, so that the rest of the subprogram can be verified
(informally or formally) using only the entry preconditions,
and the expression in this pragma. This allows dividing up
a subprogram into sections for the purposes of testing or
formal verification. The pragma also serves as useful
documentation.

Pragma Assertion_Policy
=======================

Syntax::

  pragma Assertion_Policy (CHECK | DISABLE | IGNORE | SUPPRESSIBLE);

  pragma Assertion_Policy (
      ASSERTION_KIND => POLICY_IDENTIFIER
   {, ASSERTION_KIND => POLICY_IDENTIFIER});

  ASSERTION_KIND ::= RM_ASSERTION_KIND | ID_ASSERTION_KIND

  RM_ASSERTION_KIND ::= Assert                    |
                        Static_Predicate          |
                        Dynamic_Predicate         |
                        Pre                       |
                        Pre'Class                 |
                        Post                      |
                        Post'Class                |
                        Type_Invariant            |
                        Type_Invariant'Class      |
                        Default_Initial_Condition

  ID_ASSERTION_KIND ::= Assertions           |
                        Assert_And_Cut       |
                        Assume               |
                        Contract_Cases       |
                        Debug                |
                        Ghost                |
                        Initial_Condition    |
                        Invariant            |
                        Invariant'Class      |
                        Loop_Invariant       |
                        Loop_Variant         |
                        Postcondition        |
                        Precondition         |
                        Predicate            |
                        Refined_Post         |
                        Statement_Assertions |
                        Subprogram_Variant

  POLICY_IDENTIFIER ::= Check | Disable | Ignore | Suppressible


This is a standard Ada 2012 pragma that is available as an
implementation-defined pragma in earlier versions of Ada.
The assertion kinds ``RM_ASSERTION_KIND`` are those defined in
the Ada standard. The assertion kinds ``ID_ASSERTION_KIND``
are implementation defined additions recognized by the GNAT compiler.

The pragma applies in both cases to pragmas and aspects with matching
names, e.g. ``Pre`` applies to the Pre aspect, and ``Precondition``
applies to both the ``Precondition`` pragma
and the aspect ``Precondition``. Note that the identifiers for
pragmas Pre_Class and Post_Class are Pre'Class and Post'Class (not
Pre_Class and Post_Class), since these pragmas are intended to be
identical to the corresponding aspects).

If the policy is ``CHECK``, then assertions are enabled, i.e.
the corresponding pragma or aspect is activated.
If the policy is ``IGNORE``, then assertions are ignored, i.e.
the corresponding pragma or aspect is deactivated.
This pragma overrides the effect of the *-gnata* switch on the
command line.
If the policy is ``SUPPRESSIBLE``, then assertions are enabled by default,
however, if the *-gnatp* switch is specified all assertions are ignored.

The implementation defined policy ``DISABLE`` is like
``IGNORE`` except that it completely disables semantic
checking of the corresponding pragma or aspect. This is
useful when the pragma or aspect argument references subprograms
in a with'ed package which is replaced by a dummy package
for the final build.

The implementation defined assertion kind ``Assertions`` applies to all
assertion kinds. The form with no assertion kind given implies this
choice, so it applies to all assertion kinds (RM defined, and
implementation defined).

The implementation defined assertion kind ``Statement_Assertions``
applies to ``Assert``, ``Assert_And_Cut``,
``Assume``, ``Loop_Invariant``, and ``Loop_Variant``.

Pragma Assume
=============

Syntax:

::

  pragma Assume (
    boolean_EXPRESSION
    [, string_EXPRESSION]);


The effect of this pragma is identical to that of pragma ``Assert``,
except that in an ``Assertion_Policy`` pragma, the identifier
``Assume`` is used to control whether it is ignored or checked
(or disabled).

The intention is that this be used for assumptions about the
external environment. So you cannot expect to verify formally
or informally that the condition is met, this must be
established by examining things outside the program itself.
For example, we may have code that depends on the size of
``Long_Long_Integer`` being at least 64. So we could write:

.. code-block:: ada

  pragma Assume (Long_Long_Integer'Size >= 64);


This assumption cannot be proved from the program itself,
but it acts as a useful run-time check that the assumption
is met, and documents the need to ensure that it is met by
reference to information outside the program.

Pragma Assume_No_Invalid_Values
===============================
.. index:: Invalid representations

.. index:: Invalid values

Syntax:

.. code-block:: ada

  pragma Assume_No_Invalid_Values (On | Off);


This is a configuration pragma that controls the assumptions made by the
compiler about the occurrence of invalid representations (invalid values)
in the code.

The default behavior (corresponding to an Off argument for this pragma), is
to assume that values may in general be invalid unless the compiler can
prove they are valid. Consider the following example:

.. code-block:: ada

  V1 : Integer range 1 .. 10;
  V2 : Integer range 11 .. 20;
  ...
  for J in V2 .. V1 loop
     ...
  end loop;


if V1 and V2 have valid values, then the loop is known at compile
time not to execute since the lower bound must be greater than the
upper bound. However in default mode, no such assumption is made,
and the loop may execute. If ``Assume_No_Invalid_Values (On)``
is given, the compiler will assume that any occurrence of a variable
other than in an explicit ``'Valid`` test always has a valid
value, and the loop above will be optimized away.

The use of ``Assume_No_Invalid_Values (On)`` is appropriate if
you know your code is free of uninitialized variables and other
possible sources of invalid representations, and may result in
more efficient code. A program that accesses an invalid representation
with this pragma in effect is erroneous, so no guarantees can be made
about its behavior.

It is peculiar though permissible to use this pragma in conjunction
with validity checking (-gnatVa). In such cases, accessing invalid
values will generally give an exception, though formally the program
is erroneous so there are no guarantees that this will always be the
case, and it is recommended that these two options not be used together.

.. _Pragma-Async_Readers:

Pragma Async_Readers
====================

Syntax:

.. code-block:: ada

  pragma Async_Readers [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect ``Async_Readers`` in
the SPARK 2014 Reference Manual, section 7.1.2.

.. _Pragma-Async_Writers:

Pragma Async_Writers
====================

Syntax:

.. code-block:: ada

  pragma Async_Writers [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect ``Async_Writers`` in
the SPARK 2014 Reference Manual, section 7.1.2.

Pragma Attribute_Definition
===========================

Syntax:

::

  pragma Attribute_Definition
    ([Attribute  =>] ATTRIBUTE_DESIGNATOR,
     [Entity     =>] LOCAL_NAME,
     [Expression =>] EXPRESSION | NAME);


If ``Attribute`` is a known attribute name, this pragma is equivalent to
the attribute definition clause:


.. code-block:: ada

    for Entity'Attribute use Expression;


If ``Attribute`` is not a recognized attribute name, the pragma is
ignored, and a warning is emitted. This allows source
code to be written that takes advantage of some new attribute, while remaining
compilable with earlier compilers.

Pragma C_Pass_By_Copy
=====================
.. index:: Passing by copy


Syntax:

::

  pragma C_Pass_By_Copy
    ([Max_Size =>] static_integer_EXPRESSION);


Normally the default mechanism for passing C convention records to C
convention subprograms is to pass them by reference, as suggested by RM
B.3(69).  Use the configuration pragma ``C_Pass_By_Copy`` to change
this default, by requiring that record formal parameters be passed by
copy if all of the following conditions are met:

*
  The size of the record type does not exceed the value specified for
  ``Max_Size``.
*
  The record type has ``Convention C``.
*
  The formal parameter has this record type, and the subprogram has a
  foreign (non-Ada) convention.

If these conditions are met the argument is passed by copy; i.e., in a
manner consistent with what C expects if the corresponding formal in the
C prototype is a struct (rather than a pointer to a struct).

You can also pass records by copy by specifying the convention
``C_Pass_By_Copy`` for the record type, or by using the extended
``Import`` and ``Export`` pragmas, which allow specification of
passing mechanisms on a parameter by parameter basis.

Pragma Check
============
.. index:: Assertions

.. index:: Named assertions


Syntax:

::

  pragma Check (
       [Name    =>] CHECK_KIND,
       [Check   =>] Boolean_EXPRESSION
    [, [Message =>] string_EXPRESSION] );

  CHECK_KIND ::= IDENTIFIER           |
                 Pre'Class            |
                 Post'Class           |
                 Type_Invariant'Class |
                 Invariant'Class


This pragma is similar to the predefined pragma ``Assert`` except that an
extra identifier argument is present. In conjunction with pragma
``Check_Policy``, this can be used to define groups of assertions that can
be independently controlled. The identifier ``Assertion`` is special, it
refers to the normal set of pragma ``Assert`` statements.

Checks introduced by this pragma are normally deactivated by default. They can
be activated either by the command line option *-gnata*, which turns on
all checks, or individually controlled using pragma ``Check_Policy``.

The identifiers ``Assertions`` and ``Statement_Assertions`` are not
permitted as check kinds, since this would cause confusion with the use
of these identifiers in ``Assertion_Policy`` and ``Check_Policy``
pragmas, where they are used to refer to sets of assertions.

Pragma Check_Float_Overflow
===========================
.. index:: Floating-point overflow


Syntax:

.. code-block:: ada

  pragma Check_Float_Overflow;


In Ada, the predefined floating-point types (``Short_Float``,
``Float``, ``Long_Float``, ``Long_Long_Float``) are
defined to be *unconstrained*. This means that even though each
has a well-defined base range, an operation that delivers a result
outside this base range is not required to raise an exception.
This implementation permission accommodates the notion
of infinities in IEEE floating-point, and corresponds to the
efficient execution mode on most machines. GNAT will not raise
overflow exceptions on these machines; instead it will generate
infinities and NaN's as defined in the IEEE standard.

Generating infinities, although efficient, is not always desirable.
Often the preferable approach is to check for overflow, even at the
(perhaps considerable) expense of run-time performance.
This can be accomplished by defining your own constrained floating-point subtypes -- i.e., by supplying explicit
range constraints -- and indeed such a subtype
can have the same base range as its base type. For example:


.. code-block:: ada

  subtype My_Float is Float range Float'Range;


Here ``My_Float`` has the same range as
``Float`` but is constrained, so operations on
``My_Float`` values will be checked for overflow
against this range.

This style will achieve the desired goal, but
it is often more convenient to be able to simply use
the standard predefined floating-point types as long
as overflow checking could be guaranteed.
The ``Check_Float_Overflow``
configuration pragma achieves this effect. If a unit is compiled
subject to this configuration pragma, then all operations
on predefined floating-point types including operations on
base types of these floating-point types will be treated as
though those types were constrained, and overflow checks
will be generated. The ``Constraint_Error``
exception is raised if the result is out of range.

This mode can also be set by use of the compiler
switch *-gnateF*.

Pragma Check_Name
=================
.. index:: Defining check names

.. index:: Check names, defining


Syntax:

.. code-block:: ada

  pragma Check_Name (check_name_IDENTIFIER);


This is a configuration pragma that defines a new implementation
defined check name (unless IDENTIFIER matches one of the predefined
check names, in which case the pragma has no effect). Check names
are global to a partition, so if two or more configuration pragmas
are present in a partition mentioning the same name, only one new
check name is introduced.

An implementation defined check name introduced with this pragma may
be used in only three contexts: ``pragma Suppress``,
``pragma Unsuppress``,
and as the prefix of a ``Check_Name'Enabled`` attribute reference. For
any of these three cases, the check name must be visible. A check
name is visible if it is in the configuration pragmas applying to
the current unit, or if it appears at the start of any unit that
is part of the dependency set of the current unit (e.g., units that
are mentioned in ``with`` clauses).

Check names introduced by this pragma are subject to control by compiler
switches (in particular -gnatp) in the usual manner.

Pragma Check_Policy
===================
.. index:: Controlling assertions

.. index:: Assertions, control

.. index:: Check pragma control

.. index:: Named assertions


Syntax:

::

  pragma Check_Policy
   ([Name   =>] CHECK_KIND,
    [Policy =>] POLICY_IDENTIFIER);

  pragma Check_Policy (
      CHECK_KIND => POLICY_IDENTIFIER
   {, CHECK_KIND => POLICY_IDENTIFIER});

  ASSERTION_KIND ::= RM_ASSERTION_KIND | ID_ASSERTION_KIND

  CHECK_KIND ::= IDENTIFIER           |
                 Pre'Class            |
                 Post'Class           |
                 Type_Invariant'Class |
                 Invariant'Class

  The identifiers Name and Policy are not allowed as CHECK_KIND values. This
  avoids confusion between the two possible syntax forms for this pragma.

  POLICY_IDENTIFIER ::= ON | OFF | CHECK | DISABLE | IGNORE


This pragma is used to set the checking policy for assertions (specified
by aspects or pragmas), the ``Debug`` pragma, or additional checks
to be checked using the ``Check`` pragma. It may appear either as
a configuration pragma, or within a declarative part of package. In the
latter case, it applies from the point where it appears to the end of
the declarative region (like pragma ``Suppress``).

The ``Check_Policy`` pragma is similar to the
predefined ``Assertion_Policy`` pragma,
and if the check kind corresponds to one of the assertion kinds that
are allowed by ``Assertion_Policy``, then the effect is identical.

If the first argument is Debug, then the policy applies to Debug pragmas,
disabling their effect if the policy is ``OFF``, ``DISABLE``, or
``IGNORE``, and allowing them to execute with normal semantics if
the policy is ``ON`` or ``CHECK``. In addition if the policy is
``DISABLE``, then the procedure call in ``Debug`` pragmas will
be totally ignored and not analyzed semantically.

Finally the first argument may be some other identifier than the above
possibilities, in which case it controls a set of named assertions
that can be checked using pragma ``Check``. For example, if the pragma:


.. code-block:: ada

  pragma Check_Policy (Critical_Error, OFF);


is given, then subsequent ``Check`` pragmas whose first argument is also
``Critical_Error`` will be disabled.

The check policy is ``OFF`` to turn off corresponding checks, and ``ON``
to turn on corresponding checks. The default for a set of checks for which no
``Check_Policy`` is given is ``OFF`` unless the compiler switch
*-gnata* is given, which turns on all checks by default.

The check policy settings ``CHECK`` and ``IGNORE`` are recognized
as synonyms for ``ON`` and ``OFF``. These synonyms are provided for
compatibility with the standard ``Assertion_Policy`` pragma. The check
policy setting ``DISABLE`` causes the second argument of a corresponding
``Check`` pragma to be completely ignored and not analyzed.

Pragma Comment
==============

Syntax:


.. code-block:: ada

  pragma Comment (static_string_EXPRESSION);


This is almost identical in effect to pragma ``Ident``.  It allows the
placement of a comment into the object file and hence into the
executable file if the operating system permits such usage.  The
difference is that ``Comment``, unlike ``Ident``, has
no limitations on placement of the pragma (it can be placed
anywhere in the main source unit), and if more than one pragma
is used, all comments are retained.

Pragma Common_Object
====================

Syntax:


::

  pragma Common_Object (
       [Internal =>] LOCAL_NAME
    [, [External =>] EXTERNAL_SYMBOL]
    [, [Size     =>] EXTERNAL_SYMBOL] );

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION


This pragma enables the shared use of variables stored in overlaid
linker areas corresponding to the use of ``COMMON``
in Fortran.  The single
object ``LOCAL_NAME`` is assigned to the area designated by
the ``External`` argument.
You may define a record to correspond to a series
of fields.  The ``Size`` argument
is syntax checked in GNAT, but otherwise ignored.

``Common_Object`` is not supported on all platforms.  If no
support is available, then the code generator will issue a message
indicating that the necessary attribute for implementation of this
pragma is not available.

.. _Compile_Time_Error:

Pragma Compile_Time_Error
=========================

Syntax:


.. code-block:: ada

  pragma Compile_Time_Error
           (boolean_EXPRESSION, static_string_EXPRESSION);


This pragma can be used to generate additional compile time
error messages. It
is particularly useful in generics, where errors can be issued for
specific problematic instantiations. The first parameter is a boolean
expression. The pragma ensures that the value of an expression
is known at compile time, and has the value False. The set of expressions
whose values are known at compile time includes all static boolean
expressions, and also other values which the compiler can determine
at compile time (e.g., the size of a record type set by an explicit
size representation clause, or the value of a variable which was
initialized to a constant and is known not to have been modified).
If these conditions are not met, an error message is generated using
the value given as the second argument. This string value may contain
embedded ASCII.LF characters to break the message into multiple lines.

Pragma Compile_Time_Warning
===========================

Syntax:


.. code-block:: ada

  pragma Compile_Time_Warning
           (boolean_EXPRESSION, static_string_EXPRESSION);


Same as pragma Compile_Time_Error, except a warning is issued instead
of an error message. If switch *-gnatw_C* is used, a warning is only issued
if the value of the expression is known to be True at compile time, not when
the value of the expression is not known at compile time.
Note that if this pragma is used in a package that
is with'ed by a client, the client will get the warning even though it
is issued by a with'ed package (normally warnings in with'ed units are
suppressed, but this is a special exception to that rule).

One typical use is within a generic where compile time known characteristics
of formal parameters are tested, and warnings given appropriately. Another use
with a first parameter of True is to warn a client about use of a package,
for example that it is not fully implemented.

In previous versions of the compiler, combining *-gnatwe* with
Compile_Time_Warning resulted in a fatal error. Now the compiler always emits
a warning. You can use :ref:`Compile_Time_Error` to force the generation of
an error.

Pragma Compiler_Unit
====================

Syntax:


.. code-block:: ada

  pragma Compiler_Unit;


This pragma is obsolete. It is equivalent to Compiler_Unit_Warning. It is
retained so that old versions of the GNAT run-time that use this pragma can
be compiled with newer versions of the compiler.

Pragma Compiler_Unit_Warning
============================

Syntax:


.. code-block:: ada

  pragma Compiler_Unit_Warning;


This pragma is intended only for internal use in the GNAT run-time library.
It indicates that the unit is used as part of the compiler build. The effect
is to generate warnings for the use of constructs (for example, conditional
expressions) that would cause trouble when bootstrapping using an older
version of GNAT. For the exact list of restrictions, see the compiler sources
and references to Check_Compiler_Unit.

Pragma Complete_Representation
==============================

Syntax:


.. code-block:: ada

  pragma Complete_Representation;


This pragma must appear immediately within a record representation
clause. Typical placements are before the first component clause
or after the last component clause. The effect is to give an error
message if any component is missing a component clause. This pragma
may be used to ensure that a record representation clause is
complete, and that this invariant is maintained if fields are
added to the record in the future.

Pragma Complex_Representation
=============================

Syntax:


::

  pragma Complex_Representation
          ([Entity =>] LOCAL_NAME);


The ``Entity`` argument must be the name of a record type which has
two fields of the same floating-point type.  The effect of this pragma is
to force gcc to use the special internal complex representation form for
this record, which may be more efficient.  Note that this may result in
the code for this type not conforming to standard ABI (application
binary interface) requirements for the handling of record types.  For
example, in some environments, there is a requirement for passing
records by pointer, and the use of this pragma may result in passing
this type in floating-point registers.

Pragma Component_Alignment
==========================
.. index:: Alignments of components
.. index:: Pragma Component_Alignment


Syntax:

::

  pragma Component_Alignment (
       [Form =>] ALIGNMENT_CHOICE
    [, [Name =>] type_LOCAL_NAME]);

  ALIGNMENT_CHOICE ::=
    Component_Size
  | Component_Size_4
  | Storage_Unit
  | Default


Specifies the alignment of components in array or record types.
The meaning of the ``Form`` argument is as follows:


  .. index:: Component_Size (in pragma Component_Alignment)

*Component_Size*
  Aligns scalar components and subcomponents of the array or record type
  on boundaries appropriate to their inherent size (naturally
  aligned).  For example, 1-byte components are aligned on byte boundaries,
  2-byte integer components are aligned on 2-byte boundaries, 4-byte
  integer components are aligned on 4-byte boundaries and so on.  These
  alignment rules correspond to the normal rules for C compilers on all
  machines except the VAX.

  .. index:: Component_Size_4 (in pragma Component_Alignment)

*Component_Size_4*
  Naturally aligns components with a size of four or fewer
  bytes.  Components that are larger than 4 bytes are placed on the next
  4-byte boundary.

  .. index:: Storage_Unit (in pragma Component_Alignment)

*Storage_Unit*
  Specifies that array or record components are byte aligned, i.e.,
  aligned on boundaries determined by the value of the constant
  ``System.Storage_Unit``.

  .. index:: Default (in pragma Component_Alignment)

*Default*
  Specifies that array or record components are aligned on default
  boundaries, appropriate to the underlying hardware or operating system or
  both. The ``Default`` choice is the same as ``Component_Size`` (natural
  alignment).

If the ``Name`` parameter is present, ``type_LOCAL_NAME`` must
refer to a local record or array type, and the specified alignment
choice applies to the specified type.  The use of
``Component_Alignment`` together with a pragma ``Pack`` causes the
``Component_Alignment`` pragma to be ignored.  The use of
``Component_Alignment`` together with a record representation clause
is only effective for fields not specified by the representation clause.

If the ``Name`` parameter is absent, the pragma can be used as either
a configuration pragma, in which case it applies to one or more units in
accordance with the normal rules for configuration pragmas, or it can be
used within a declarative part, in which case it applies to types that
are declared within this declarative part, or within any nested scope
within this declarative part.  In either case it specifies the alignment
to be applied to any record or array type which has otherwise standard
representation.

If the alignment for a record or array type is not specified (using
pragma ``Pack``, pragma ``Component_Alignment``, or a record rep
clause), the GNAT uses the default alignment as described previously.

.. _Pragma-Constant_After_Elaboration:

Pragma Constant_After_Elaboration
=================================

Syntax:

.. code-block:: ada

  pragma Constant_After_Elaboration [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect
``Constant_After_Elaboration`` in the SPARK 2014 Reference Manual, section 3.3.1.

.. _Pragma-Contract_Cases:

Pragma Contract_Cases
=====================
.. index:: Contract cases

Syntax:

.. code-block:: ada

  pragma Contract_Cases ((CONTRACT_CASE {, CONTRACT_CASE));

  CONTRACT_CASE ::= CASE_GUARD => CONSEQUENCE

  CASE_GUARD ::= boolean_EXPRESSION | others

  CONSEQUENCE ::= boolean_EXPRESSION

The ``Contract_Cases`` pragma allows defining fine-grain specifications
that can complement or replace the contract given by a precondition and a
postcondition. Additionally, the ``Contract_Cases`` pragma can be used
by testing and formal verification tools. The compiler checks its validity and,
depending on the assertion policy at the point of declaration of the pragma,
it may insert a check in the executable. For code generation, the contract
cases


.. code-block:: ada

  pragma Contract_Cases (
    Cond1 => Pred1,
    Cond2 => Pred2);


are equivalent to


.. code-block:: ada

  C1 : constant Boolean := Cond1;  --  evaluated at subprogram entry
  C2 : constant Boolean := Cond2;  --  evaluated at subprogram entry
  pragma Precondition ((C1 and not C2) or (C2 and not C1));
  pragma Postcondition (if C1 then Pred1);
  pragma Postcondition (if C2 then Pred2);


The precondition ensures that one and only one of the case guards is
satisfied on entry to the subprogram.
The postcondition ensures that for the case guard that was True on entry,
the corresponding consequence is True on exit. Other consequence expressions
are not evaluated.

A precondition ``P`` and postcondition ``Q`` can also be
expressed as contract cases:

.. code-block:: ada

  pragma Contract_Cases (P => Q);


The placement and visibility rules for ``Contract_Cases`` pragmas are
identical to those described for preconditions and postconditions.

The compiler checks that boolean expressions given in case guards and
consequences are valid, where the rules for case guards are the same as
the rule for an expression in ``Precondition`` and the rules for
consequences are the same as the rule for an expression in
``Postcondition``. In particular, attributes ``'Old`` and
``'Result`` can only be used within consequence expressions.
The case guard for the last contract case may be ``others``, to denote
any case not captured by the previous cases. The
following is an example of use within a package spec:


.. code-block:: ada

  package Math_Functions is
     ...
     function Sqrt (Arg : Float) return Float;
     pragma Contract_Cases (((Arg in 0.0 .. 99.0) => Sqrt'Result < 10.0,
                             Arg >= 100.0         => Sqrt'Result >= 10.0,
                             others               => Sqrt'Result = 0.0));
     ...
  end Math_Functions;


The meaning of contract cases is that only one case should apply at each
call, as determined by the corresponding case guard evaluating to True,
and that the consequence for this case should hold when the subprogram
returns.

Pragma Convention_Identifier
============================
.. index:: Conventions, synonyms

Syntax:


::

  pragma Convention_Identifier (
           [Name =>]       IDENTIFIER,
           [Convention =>] convention_IDENTIFIER);


This pragma provides a mechanism for supplying synonyms for existing
convention identifiers. The ``Name`` identifier can subsequently
be used as a synonym for the given convention in other pragmas (including
for example pragma ``Import`` or another ``Convention_Identifier``
pragma). As an example of the use of this, suppose you had legacy code
which used Fortran77 as the identifier for Fortran. Then the pragma:


.. code-block:: ada

  pragma Convention_Identifier (Fortran77, Fortran);


would allow the use of the convention identifier ``Fortran77`` in
subsequent code, avoiding the need to modify the sources. As another
example, you could use this to parameterize convention requirements
according to systems. Suppose you needed to use ``Stdcall`` on
windows systems, and ``C`` on some other system, then you could
define a convention identifier ``Library`` and use a single
``Convention_Identifier`` pragma to specify which convention
would be used system-wide.

Pragma CPP_Class
================
.. index:: Interfacing with C++

Syntax:


::

  pragma CPP_Class ([Entity =>] LOCAL_NAME);


The argument denotes an entity in the current declarative region that is
declared as a record type. It indicates that the type corresponds to an
externally declared C++ class type, and is to be laid out the same way
that C++ would lay out the type. If the C++ class has virtual primitives
then the record must be declared as a tagged record type.

Types for which ``CPP_Class`` is specified do not have assignment or
equality operators defined (such operations can be imported or declared
as subprograms as required). Initialization is allowed only by constructor
functions (see pragma ``CPP_Constructor``). Such types are implicitly
limited if not explicitly declared as limited or derived from a limited
type, and an error is issued in that case.

See :ref:`Interfacing_to_C++` for related information.

Note: Pragma ``CPP_Class`` is currently obsolete. It is supported
for backward compatibility but its functionality is available
using pragma ``Import`` with ``Convention`` = ``CPP``.

Pragma CPP_Constructor
======================
.. index:: Interfacing with C++


Syntax:


::

  pragma CPP_Constructor ([Entity =>] LOCAL_NAME
    [, [External_Name =>] static_string_EXPRESSION ]
    [, [Link_Name     =>] static_string_EXPRESSION ]);


This pragma identifies an imported function (imported in the usual way
with pragma ``Import``) as corresponding to a C++ constructor. If
``External_Name`` and ``Link_Name`` are not specified then the
``Entity`` argument is a name that must have been previously mentioned
in a pragma ``Import`` with ``Convention`` = ``CPP``. Such name
must be of one of the following forms:

*
  **function** ``Fname`` **return** T`

*
  **function** ``Fname`` **return** T'Class

*
  **function** ``Fname`` (...) **return** T`

*
  **function** ``Fname`` (...) **return** T'Class

where ``T`` is a limited record type imported from C++ with pragma
``Import`` and ``Convention`` = ``CPP``.

The first two forms import the default constructor, used when an object
of type ``T`` is created on the Ada side with no explicit constructor.
The latter two forms cover all the non-default constructors of the type.
See the GNAT User's Guide for details.

If no constructors are imported, it is impossible to create any objects
on the Ada side and the type is implicitly declared abstract.

Pragma ``CPP_Constructor`` is intended primarily for automatic generation
using an automatic binding generator tool (such as the :switch:`-fdump-ada-spec`
GCC switch).
See :ref:`Interfacing_to_C++` for more related information.

Note: The use of functions returning class-wide types for constructors is
currently obsolete. They are supported for backward compatibility. The
use of functions returning the type T leave the Ada sources more clear
because the imported C++ constructors always return an object of type T;
that is, they never return an object whose type is a descendant of type T.

Pragma CPP_Virtual
==================
.. index:: Interfacing to C++


This pragma is now obsolete and, other than generating a warning if warnings
on obsolescent features are enabled, is completely ignored.
It is retained for compatibility
purposes. It used to be required to ensure compoatibility with C++, but
is no longer required for that purpose because GNAT generates
the same object layout as the G++ compiler by default.

See :ref:`Interfacing_to_C++` for related information.

Pragma CPP_Vtable
=================
.. index:: Interfacing with C++


This pragma is now obsolete and, other than generating a warning if warnings
on obsolescent features are enabled, is completely ignored.
It used to be required to ensure compatibility with C++, but
is no longer required for that purpose because GNAT generates
the same object layout as the G++ compiler by default.

See :ref:`Interfacing_to_C++` for related information.

Pragma CPU
==========

Syntax:


.. code-block:: ada

  pragma CPU (EXPRESSION);


This pragma is standard in Ada 2012, but is available in all earlier
versions of Ada as an implementation-defined pragma.
See Ada 2012 Reference Manual for details.

Pragma Deadline_Floor
=====================

Syntax:


.. code-block:: ada

  pragma Deadline_Floor (time_span_EXPRESSION);


This pragma applies only to protected types and specifies the floor
deadline inherited by a task when the task enters a protected object.
It is effective only when the EDF scheduling policy is used.

.. _Pragma-Default_Initial_Condition:

Pragma Default_Initial_Condition
================================

Syntax:

.. code-block:: ada

  pragma Default_Initial_Condition [ (null | boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect
``Default_Initial_Condition`` in the SPARK 2014 Reference Manual, section 7.3.3.

Pragma Debug
============

Syntax:


::

  pragma Debug ([CONDITION, ]PROCEDURE_CALL_WITHOUT_SEMICOLON);

  PROCEDURE_CALL_WITHOUT_SEMICOLON ::=
    PROCEDURE_NAME
  | PROCEDURE_PREFIX ACTUAL_PARAMETER_PART


The procedure call argument has the syntactic form of an expression, meeting
the syntactic requirements for pragmas.

If debug pragmas are not enabled or if the condition is present and evaluates
to False, this pragma has no effect. If debug pragmas are enabled, the
semantics of the pragma is exactly equivalent to the procedure call statement
corresponding to the argument with a terminating semicolon. Pragmas are
permitted in sequences of declarations, so you can use pragma ``Debug`` to
intersperse calls to debug procedures in the middle of declarations. Debug
pragmas can be enabled either by use of the command line switch *-gnata*
or by use of the pragma ``Check_Policy`` with a first argument of
``Debug``.

Pragma Debug_Policy
===================

Syntax:


.. code-block:: ada

  pragma Debug_Policy (CHECK | DISABLE | IGNORE | ON | OFF);


This pragma is equivalent to a corresponding ``Check_Policy`` pragma
with a first argument of ``Debug``. It is retained for historical
compatibility reasons.

Pragma Default_Scalar_Storage_Order
===================================
.. index:: Default_Scalar_Storage_Order

.. index:: Scalar_Storage_Order


Syntax:


.. code-block:: ada

  pragma Default_Scalar_Storage_Order (High_Order_First | Low_Order_First);


Normally if no explicit ``Scalar_Storage_Order`` is given for a record
type or array type, then the scalar storage order defaults to the ordinary
default for the target. But this default may be overridden using this pragma.
The pragma may appear as a configuration pragma, or locally within a package
spec or declarative part. In the latter case, it applies to all subsequent
types declared within that package spec or declarative part.

The following example shows the use of this pragma:


.. code-block:: ada

  pragma Default_Scalar_Storage_Order (High_Order_First);
  with System; use System;
  package DSSO1 is
     type H1 is record
        a : Integer;
     end record;

     type L2 is record
        a : Integer;
     end record;
     for L2'Scalar_Storage_Order use Low_Order_First;

     type L2a is new L2;

     package Inner is
        type H3 is record
           a : Integer;
        end record;

        pragma Default_Scalar_Storage_Order (Low_Order_First);

        type L4 is record
           a : Integer;
        end record;
     end Inner;

     type H4a is new Inner.L4;

     type H5 is record
        a : Integer;
     end record;
  end DSSO1;


In this example record types with names starting with *L* have `Low_Order_First` scalar
storage order, and record types with names starting with *H* have ``High_Order_First``.
Note that in the case of ``H4a``, the order is not inherited
from the parent type. Only an explicitly set ``Scalar_Storage_Order``
gets inherited on type derivation.

If this pragma is used as a configuration pragma which appears within a
configuration pragma file (as opposed to appearing explicitly at the start
of a single unit), then the binder will require that all units in a partition
be compiled in a similar manner, other than run-time units, which are not
affected by this pragma. Note that the use of this form is discouraged because
it may significantly degrade the run-time performance of the software, instead
the default scalar storage order ought to be changed only on a local basis.

Pragma Default_Storage_Pool
===========================
.. index:: Default_Storage_Pool


Syntax:


.. code-block:: ada

  pragma Default_Storage_Pool (storage_pool_NAME | null);


This pragma is standard in Ada 2012, but is available in all earlier
versions of Ada as an implementation-defined pragma.
See Ada 2012 Reference Manual for details.

.. _Pragma-Depends:

Pragma Depends
==============

Syntax:

.. code-block:: ada

  pragma Depends (DEPENDENCY_RELATION);

  DEPENDENCY_RELATION ::=
       null
    | (DEPENDENCY_CLAUSE {, DEPENDENCY_CLAUSE})

  DEPENDENCY_CLAUSE ::=
      OUTPUT_LIST =>[+] INPUT_LIST
    | NULL_DEPENDENCY_CLAUSE

  NULL_DEPENDENCY_CLAUSE ::= null => INPUT_LIST

  OUTPUT_LIST ::= OUTPUT | (OUTPUT {, OUTPUT})

  INPUT_LIST ::= null | INPUT | (INPUT {, INPUT})

  OUTPUT ::= NAME | FUNCTION_RESULT
  INPUT  ::= NAME

  where FUNCTION_RESULT is a function Result attribute_reference

For the semantics of this pragma, see the entry for aspect ``Depends`` in the
SPARK 2014 Reference Manual, section 6.1.5.

Pragma Detect_Blocking
======================

Syntax:

.. code-block:: ada

  pragma Detect_Blocking;


This is a standard pragma in Ada 2005, that is available in all earlier
versions of Ada as an implementation-defined pragma.

This is a configuration pragma that forces the detection of potentially
blocking operations within a protected operation, and to raise Program_Error
if that happens.

Pragma Disable_Atomic_Synchronization
=====================================

.. index:: Atomic Synchronization

Syntax:

::

  pragma Disable_Atomic_Synchronization [(Entity)];


Ada requires that accesses (reads or writes) of an atomic variable be
regarded as synchronization points in the case of multiple tasks.
Particularly in the case of multi-processors this may require special
handling, e.g. the generation of memory barriers. This capability may
be turned off using this pragma in cases where it is known not to be
required.

The placement and scope rules for this pragma are the same as those
for ``pragma Suppress``. In particular it can be used as a
configuration  pragma, or in a declaration sequence where it applies
till the end of the scope. If an ``Entity`` argument is present,
the action applies only to that entity.

Pragma Dispatching_Domain
=========================

Syntax:


.. code-block:: ada

  pragma Dispatching_Domain (EXPRESSION);


This pragma is standard in Ada 2012, but is available in all earlier
versions of Ada as an implementation-defined pragma.
See Ada 2012 Reference Manual for details.

.. _Pragma-Effective_Reads:

Pragma Effective_Reads
======================

Syntax:

.. code-block:: ada

  pragma Effective_Reads [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect ``Effective_Reads`` in
the SPARK 2014 Reference Manual, section 7.1.2.

.. _Pragma-Effective_Writes:

Pragma Effective_Writes
=======================

Syntax:

.. code-block:: ada

  pragma Effective_Writes [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect ``Effective_Writes``
in the SPARK 2014 Reference Manual, section 7.1.2.

Pragma Elaboration_Checks
=========================
.. index:: Elaboration control


Syntax:


.. code-block:: ada

  pragma Elaboration_Checks (Dynamic | Static);


This is a configuration pragma which specifies the elaboration model to be
used during compilation. For more information on the elaboration models of
GNAT, consult the chapter on elaboration order handling in the *GNAT User's
Guide*.

The pragma may appear in the following contexts:

* Configuration pragmas file

* Prior to the context clauses of a compilation unit's initial declaration

Any other placement of the pragma will result in a warning and the effects of
the offending pragma will be ignored.

If the pragma argument is ``Dynamic``, then the dynamic elaboration model is in
effect. If the pragma argument is ``Static``, then the static elaboration model
is in effect.

Pragma Eliminate
================
.. index:: Elimination of unused subprograms


Syntax:


::

   pragma Eliminate (
               [  Unit_Name       => ] IDENTIFIER | SELECTED_COMPONENT ,
               [  Entity          => ] IDENTIFIER |
                                       SELECTED_COMPONENT |
                                       STRING_LITERAL
               [, Source_Location =>   SOURCE_TRACE ] );

           SOURCE_TRACE    ::= STRING_LITERAL


This pragma indicates that the given entity is not used in the program to be
compiled and built, thus allowing the compiler to
eliminate the code or data associated with the named entity. Any reference to
an eliminated entity causes a compile-time or link-time error.

The pragma has the following semantics, where ``U`` is the unit specified by
the ``Unit_Name`` argument and ``E`` is the entity specified by the ``Entity``
argument:

*  ``E`` must be a subprogram that is explicitly declared either:

   o  Within ``U``, or

   o  Within a generic package that is instantiated in ``U``, or

   o  As an instance of generic subprogram instantiated in ``U``.

   Otherwise the pragma is ignored.

*  If ``E`` is overloaded within ``U`` then, in the absence of a
   ``Source_Location`` argument, all overloadings are eliminated.

*  If ``E`` is overloaded within ``U`` and only some overloadings
   are to be eliminated, then each overloading to be eliminated
   must be specified in a corresponding pragma ``Eliminate``
   with a ``Source_Location`` argument identifying the line where the
   declaration appears, as described below.

*  If ``E`` is declared as the result of a generic instantiation, then
   a ``Source_Location`` argument is needed, as described below

Pragma ``Eliminate`` allows a program to be compiled in a system-independent
manner, so that unused entities are eliminated but without
needing to modify the source text. Normally the required set of
``Eliminate`` pragmas is constructed automatically using the ``gnatelim`` tool.

Any source file change that removes, splits, or
adds lines may make the set of ``Eliminate`` pragmas invalid because their
``Source_Location`` argument values may get out of date.

Pragma ``Eliminate`` may be used where the referenced entity is a dispatching
operation. In this case all the subprograms to which the given operation can
dispatch are considered to be unused (are never called as a result of a direct
or a dispatching call).

The string literal given for the source location specifies the line number
of the declaration of the entity, using the following syntax for ``SOURCE_TRACE``:

::

   SOURCE_TRACE     ::= SOURCE_REFERENCE [ LBRACKET SOURCE_TRACE RBRACKET ]

   LBRACKET         ::= '['
   RBRACKET         ::= ']'

   SOURCE_REFERENCE ::= FILE_NAME : LINE_NUMBER

   LINE_NUMBER      ::= DIGIT {DIGIT}


Spaces around the colon in a ``SOURCE_REFERENCE`` are optional.

The source trace that is given as the ``Source_Location`` must obey the
following rules (or else the pragma is ignored), where ``U`` is
the unit ``U`` specified by the ``Unit_Name`` argument and ``E`` is the
subprogram specified by the ``Entity`` argument:

*  ``FILE_NAME`` is the short name (with no directory
   information) of the Ada source file for ``U``, using the required syntax
   for the underlying file system (e.g. case is significant if the underlying
   operating system is case sensitive).
   If ``U`` is a package and ``E`` is a subprogram declared in the package
   specification and its full declaration appears in the package body,
   then the  relevant source file is the one for the package specification;
   analogously if ``U`` is a generic package.

*  If ``E`` is not declared in a generic instantiation (this includes
   generic subprogram instances), the source trace includes only one source
   line reference. ``LINE_NUMBER`` gives the line number of the occurrence
   of the declaration of ``E`` within the source file (as a decimal literal
   without an exponent or point).

*  If ``E`` is declared by a generic instantiation, its source trace
   (from left to right) starts with the source location of the
   declaration of ``E`` in the generic unit and ends with the source
   location of the instantiation, given in square brackets. This approach is
   applied recursively with nested instantiations: the rightmost (nested
   most deeply in square brackets) element of the source trace is the location
   of the outermost instantiation, and the leftmost element (that is, outside
   of any square brackets) is the location of the declaration of ``E`` in
   the generic unit.

Examples:

   .. code-block:: ada

      pragma Eliminate (Pkg0, Proc);
      -- Eliminate (all overloadings of) Proc in Pkg0

      pragma Eliminate (Pkg1, Proc,
                        Source_Location => "pkg1.ads:8");
      -- Eliminate overloading of Proc at line 8 in pkg1.ads

      -- Assume the following file contents:
      --   gen_pkg.ads
      --   1: generic
      --   2:   type T is private;
      --   3: package Gen_Pkg is
      --   4:   procedure Proc(N : T);
      --  ...   ...
      --  ... end Gen_Pkg;
      --
      --    q.adb
      --   1: with Gen_Pkg;
      --   2: procedure Q is
      --   3:   package Inst_Pkg is new Gen_Pkg(Integer);
      --  ...   -- No calls on Inst_Pkg.Proc
      --  ... end Q;

      -- The following pragma eliminates Inst_Pkg.Proc from Q
      pragma Eliminate (Q, Proc,
                        Source_Location => "gen_pkg.ads:4[q.adb:3]");



Pragma Enable_Atomic_Synchronization
====================================
.. index:: Atomic Synchronization


Syntax:


::

  pragma Enable_Atomic_Synchronization [(Entity)];


Ada requires that accesses (reads or writes) of an atomic variable be
regarded as synchronization points in the case of multiple tasks.
Particularly in the case of multi-processors this may require special
handling, e.g. the generation of memory barriers. This synchronization
is performed by default, but can be turned off using
``pragma Disable_Atomic_Synchronization``. The
``Enable_Atomic_Synchronization`` pragma can be used to turn
it back on.

The placement and scope rules for this pragma are the same as those
for ``pragma Unsuppress``. In particular it can be used as a
configuration  pragma, or in a declaration sequence where it applies
till the end of the scope. If an ``Entity`` argument is present,
the action applies only to that entity.

Pragma Export_Function
======================
.. index:: Argument passing mechanisms


Syntax:


::

  pragma Export_Function (
       [Internal         =>] LOCAL_NAME
    [, [External         =>] EXTERNAL_SYMBOL]
    [, [Parameter_Types  =>] PARAMETER_TYPES]
    [, [Result_Type      =>] result_SUBTYPE_MARK]
    [, [Mechanism        =>] MECHANISM]
    [, [Result_Mechanism =>] MECHANISM_NAME]);

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION
  | ""

  PARAMETER_TYPES ::=
    null
  | TYPE_DESIGNATOR {, TYPE_DESIGNATOR}

  TYPE_DESIGNATOR ::=
    subtype_NAME
  | subtype_Name ' Access

  MECHANISM ::=
    MECHANISM_NAME
  | (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})

  MECHANISM_ASSOCIATION ::=
    [formal_parameter_NAME =>] MECHANISM_NAME

  MECHANISM_NAME ::= Value | Reference


Use this pragma to make a function externally callable and optionally
provide information on mechanisms to be used for passing parameter and
result values.  We recommend, for the purposes of improving portability,
this pragma always be used in conjunction with a separate pragma
``Export``, which must precede the pragma ``Export_Function``.
GNAT does not require a separate pragma ``Export``, but if none is
present, ``Convention Ada`` is assumed, which is usually
not what is wanted, so it is usually appropriate to use this
pragma in conjunction with a ``Export`` or ``Convention``
pragma that specifies the desired foreign convention.
Pragma ``Export_Function``
(and ``Export``, if present) must appear in the same declarative
region as the function to which they apply.

The ``internal_name`` must uniquely designate the function to which the
pragma applies.  If more than one function name exists of this name in
the declarative part you must use the ``Parameter_Types`` and
``Result_Type`` parameters to achieve the required
unique designation.  The `subtype_mark`\ s in these parameters must
exactly match the subtypes in the corresponding function specification,
using positional notation to match parameters with subtype marks.
The form with an ``'Access`` attribute can be used to match an
anonymous access parameter.

.. index:: Suppressing external name

Special treatment is given if the EXTERNAL is an explicit null
string or a static string expressions that evaluates to the null
string. In this case, no external name is generated. This form
still allows the specification of parameter mechanisms.

Pragma Export_Object
====================

Syntax:


::

  pragma Export_Object
        [Internal =>] LOCAL_NAME
     [, [External =>] EXTERNAL_SYMBOL]
     [, [Size     =>] EXTERNAL_SYMBOL]

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION


This pragma designates an object as exported, and apart from the
extended rules for external symbols, is identical in effect to the use of
the normal ``Export`` pragma applied to an object.  You may use a
separate Export pragma (and you probably should from the point of view
of portability), but it is not required.  ``Size`` is syntax checked,
but otherwise ignored by GNAT.

Pragma Export_Procedure
=======================

Syntax:


::

  pragma Export_Procedure (
       [Internal        =>] LOCAL_NAME
    [, [External        =>] EXTERNAL_SYMBOL]
    [, [Parameter_Types =>] PARAMETER_TYPES]
    [, [Mechanism       =>] MECHANISM]);

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION
  | ""

  PARAMETER_TYPES ::=
    null
  | TYPE_DESIGNATOR {, TYPE_DESIGNATOR}

  TYPE_DESIGNATOR ::=
    subtype_NAME
  | subtype_Name ' Access

  MECHANISM ::=
    MECHANISM_NAME
  | (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})

  MECHANISM_ASSOCIATION ::=
    [formal_parameter_NAME =>] MECHANISM_NAME

  MECHANISM_NAME ::= Value | Reference


This pragma is identical to ``Export_Function`` except that it
applies to a procedure rather than a function and the parameters
``Result_Type`` and ``Result_Mechanism`` are not permitted.
GNAT does not require a separate pragma ``Export``, but if none is
present, ``Convention Ada`` is assumed, which is usually
not what is wanted, so it is usually appropriate to use this
pragma in conjunction with a ``Export`` or ``Convention``
pragma that specifies the desired foreign convention.

.. index:: Suppressing external name

Special treatment is given if the EXTERNAL is an explicit null
string or a static string expressions that evaluates to the null
string. In this case, no external name is generated. This form
still allows the specification of parameter mechanisms.

Pragma Export_Valued_Procedure
==============================

Syntax:


::

  pragma Export_Valued_Procedure (
       [Internal        =>] LOCAL_NAME
    [, [External        =>] EXTERNAL_SYMBOL]
    [, [Parameter_Types =>] PARAMETER_TYPES]
    [, [Mechanism       =>] MECHANISM]);

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION
  | ""

  PARAMETER_TYPES ::=
    null
  | TYPE_DESIGNATOR {, TYPE_DESIGNATOR}

  TYPE_DESIGNATOR ::=
    subtype_NAME
  | subtype_Name ' Access

  MECHANISM ::=
    MECHANISM_NAME
  | (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})

  MECHANISM_ASSOCIATION ::=
    [formal_parameter_NAME =>] MECHANISM_NAME

  MECHANISM_NAME ::= Value | Reference


This pragma is identical to ``Export_Procedure`` except that the
first parameter of ``LOCAL_NAME``, which must be present, must be of
mode ``out``, and externally the subprogram is treated as a function
with this parameter as the result of the function.  GNAT provides for
this capability to allow the use of ``out`` and ``in out``
parameters in interfacing to external functions (which are not permitted
in Ada functions).
GNAT does not require a separate pragma ``Export``, but if none is
present, ``Convention Ada`` is assumed, which is almost certainly
not what is wanted since the whole point of this pragma is to interface
with foreign language functions, so it is usually appropriate to use this
pragma in conjunction with a ``Export`` or ``Convention``
pragma that specifies the desired foreign convention.

.. index:: Suppressing external name

Special treatment is given if the EXTERNAL is an explicit null
string or a static string expressions that evaluates to the null
string. In this case, no external name is generated. This form
still allows the specification of parameter mechanisms.

Pragma Extend_System
====================
.. index:: System, extending

.. index:: DEC Ada 83


Syntax:


::

  pragma Extend_System ([Name =>] IDENTIFIER);


This pragma is used to provide backwards compatibility with other
implementations that extend the facilities of package ``System``.  In
GNAT, ``System`` contains only the definitions that are present in
the Ada RM.  However, other implementations, notably the DEC Ada 83
implementation, provide many extensions to package ``System``.

For each such implementation accommodated by this pragma, GNAT provides a
package :samp:`Aux_{xxx}`, e.g., ``Aux_DEC`` for the DEC Ada 83
implementation, which provides the required additional definitions.  You
can use this package in two ways.  You can ``with`` it in the normal
way and access entities either by selection or using a ``use``
clause.  In this case no special processing is required.

However, if existing code contains references such as
:samp:`System.{xxx}` where *xxx* is an entity in the extended
definitions provided in package ``System``, you may use this pragma
to extend visibility in ``System`` in a non-standard way that
provides greater compatibility with the existing code.  Pragma
``Extend_System`` is a configuration pragma whose single argument is
the name of the package containing the extended definition
(e.g., ``Aux_DEC`` for the DEC Ada case).  A unit compiled under
control of this pragma will be processed using special visibility
processing that looks in package :samp:`System.Aux_{xxx}` where
:samp:`Aux_{xxx}` is the pragma argument for any entity referenced in
package ``System``, but not found in package ``System``.

You can use this pragma either to access a predefined ``System``
extension supplied with the compiler, for example ``Aux_DEC`` or
you can construct your own extension unit following the above
definition.  Note that such a package is a child of ``System``
and thus is considered part of the implementation.
To compile it you will have to use the *-gnatg* switch
for compiling System units, as explained in the
GNAT User's Guide.

Pragma Extensions_Allowed
=========================
.. index:: Ada Extensions

.. index:: GNAT Extensions


Syntax:

.. code-block:: ada

  pragma Extensions_Allowed (On | Off);


This configuration pragma enables or disables the implementation
extension mode (the use of Off as a parameter cancels the effect
of the *-gnatX* command switch).

In extension mode, the latest version of the Ada language is
implemented (currently Ada 2022), and in addition a number
of GNAT specific extensions are recognized as follows:

* Constrained attribute for generic objects

  The ``Constrained`` attribute is permitted for objects of
  generic types. The result indicates if the corresponding actual
  is constrained.

* ``Static`` aspect on intrinsic functions

  The Ada 202x ``Static`` aspect can be specified on Intrinsic imported
  functions and the compiler will evaluate some of these intrinsic statically,
  in particular the ``Shift_Left`` and ``Shift_Right`` intrinsics.

* ``'Reduce`` attribute

  This attribute part of the Ada 202x language definition is provided for
  now under -gnatX to confirm and potentially refine its usage and syntax.

* ``[]`` aggregates

  This new aggregate syntax for arrays and containers is provided under -gnatX
  to experiment and confirm this new language syntax.

* Additional ``when`` constructs

  In addition to the ``exit when CONDITION`` control structure, several
  additional constructs are allowed following this format. Including
  ``return when CONDITION``, ``goto when CONDITION``, and
  ``raise [with EXCEPTION_MESSAGE] when CONDITION.``

  Some examples:

  .. code-block:: ada

      return Result when Variable > 10;

      raise Program_Error with "Element is null" when Element = null;

      goto End_Of_Subprogram when Variable = -1;

* Casing on composite values (aka pattern matching)

  The selector for a case statement may be of a composite type, subject to
  some restrictions (described below). Aggregate syntax is used for choices
  of such a case statement; however, in cases where a "normal" aggregate would
  require a discrete value, a discrete subtype may be used instead; box
  notation can also be used to match all values.

  Consider this example:

  .. code-block:: ada

      type Rec is record
         F1, F2 : Integer;
      end record;

      procedure Caser_1 (X : Rec) is
      begin
         case X is
            when (F1 => Positive, F2 => Positive) =>
               Do_This;
            when (F1 => Natural, F2 => <>) | (F1 => <>, F2 => Natural) =>
               Do_That;
            when others =>
                Do_The_Other_Thing;
         end case;
      end Caser_1;

  If Caser_1 is called and both components of X are positive, then
  Do_This will be called; otherwise, if either component is nonnegative
  then Do_That will be called; otherwise, Do_The_Other_Thing will be called.

  If the set of values that match the choice(s) of an earlier alternative
  overlaps the corresponding set of a later alternative, then the first
  set shall be a proper subset of the second (and the later alternative
  will not be executed if the earlier alternative "matches"). All possible
  values of the composite type shall be covered. The composite type of the
  selector shall be a nonlimited untagged (but possibly discriminated)
  record type, all of whose subcomponent subtypes are either static discrete
  subtypes or record types that meet the same restrictions.

  Support for casing on arrays (and on records that contain arrays) is
  currently subject to some restrictions. Non-positional
  array aggregates are not supported as (or within) case choices. Likewise
  for array type and subtype names. The current implementation exceeds
  compile-time capacity limits in some annoyingly common scenarios; the
  message generated in such cases is usually "Capacity exceeded in compiling
  case statement with composite selector type".

  In addition, pattern bindings are supported. This is a mechanism
  for binding a name to a component of a matching value for use within
  an alternative of a case statement. For a component association
  that occurs within a case choice, the expression may be followed by
  "is <identifier>". In the special case of a "box" component association,
  the identifier may instead be provided within the box. Either of these
  indicates that the given identifer denotes (a constant view of) the matching
  subcomponent of the case selector. Binding is not yet supported for arrays
  or subcomponents thereof.

  Consider this example (which uses type Rec from the previous example):

  .. code-block:: ada

      procedure Caser_2 (X : Rec) is
      begin
         case X is
            when (F1 => Positive is Abc, F2 => Positive) =>
               Do_This (Abc)
            when (F1 => Natural is N1, F2 => <N2>) |
                 (F1 => <N2>, F2 => Natural is N1) =>
               Do_That (Param_1 => N1, Param_2 => N2);
            when others =>
               Do_The_Other_Thing;
         end case;
      end Caser_2;

  This example is the same as the previous one with respect to
  determining whether Do_This, Do_That, or Do_The_Other_Thing will
  be called. But for this version, Do_This takes a parameter and Do_That
  takes two parameters. If Do_This is called, the actual parameter in the
  call will be X.F1.

  If Do_That is called, the situation is more complex because there are two
  choices for that alternative. If Do_That is called because the first choice
  matched (i.e., because X.F1 is nonnegative and either X.F1 or X.F2 is zero
  or negative), then the actual parameters of the call will be (in order)
  X.F1 and X.F2. If Do_That is called because the second choice matched (and
  the first one did not), then the actual parameters will be reversed.

  Within the choice list for single alternative, each choice must
  define the same set of bindings and the component subtypes for
  for a given identifer must all statically match. Currently, the case
  of a binding for a nondiscrete component is not implemented.

* Fixed lower bounds for array types and subtypes

  Unconstrained array types and subtypes can be specified with a lower bound
  that is fixed to a certain value, by writing an index range that uses the
  syntax "<lower-bound-expression> .. <>". This guarantees that all objects
  of the type or subtype will have the specified lower bound.

  For example, a matrix type with fixed lower bounds of zero for each
  dimension can be declared by the following:

  .. code-block:: ada

      type Matrix is
        array (Natural range 0 .. <>, Natural range 0 .. <>) of Integer;

  Objects of type Matrix declared with an index constraint must have index
  ranges starting at zero:

  .. code-block:: ada

      M1 : Matrix (0 .. 9, 0 .. 19);
      M2 : Matrix (2 .. 11, 3 .. 22);  -- Warning about bounds; will raise CE

  Similarly, a subtype of String can be declared that specifies the lower
  bound of objects of that subtype to be 1:

   .. code-block:: ada

      subtype String_1 is String (1 .. <>);

  If a string slice is passed to a formal of subtype String_1 in a call to
  a subprogram S, the slice's bounds will "slide" so that the lower bound
  is 1. Within S, the lower bound of the formal is known to be 1, so, unlike
  a normal unconstrained String formal, there is no need to worry about
  accounting for other possible lower-bound values. Sliding of bounds also
  occurs in other contexts, such as for object declarations with an
  unconstrained subtype with fixed lower bound, as well as in subtype
  conversions.

  Use of this feature increases safety by simplifying code, and can also
  improve the efficiency of indexing operations, since the compiler statically
  knows the lower bound of unconstrained array formals when the formal's
  subtype has index ranges with static fixed lower bounds.

* Prefixed-view notation for calls to primitive subprograms of untagged types

  Since Ada 2005, calls to primitive subprograms of a tagged type that
  have a "prefixed view" (see RM 4.1.3(9.2)) have been allowed to be
  written using the form of a selected_component, with the first actual
  parameter given as the prefix and the name of the subprogram as a
  selector. This prefixed-view notation for calls is extended so as to
  also allow such syntax for calls to primitive subprograms of untagged
  types. The primitives of an untagged type T that have a prefixed view
  are those where the first formal parameter of the subprogram either
  is of type T or is an anonymous access parameter whose designated type
  is T. For a type that has a component that happens to have the same
  simple name as one of the type's primitive subprograms, where the
  component is visible at the point of a selected_component using that
  name, preference is given to the component in a selected_component
  (as is currently the case for tagged types with such component names).

.. _Pragma-Extensions_Visible:

Pragma Extensions_Visible
=========================

Syntax:

.. code-block:: ada

  pragma Extensions_Visible [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect ``Extensions_Visible``
in the SPARK 2014 Reference Manual, section 6.1.7.

Pragma External
===============

Syntax:


::

  pragma External (
    [   Convention    =>] convention_IDENTIFIER,
    [   Entity        =>] LOCAL_NAME
    [, [External_Name =>] static_string_EXPRESSION ]
    [, [Link_Name     =>] static_string_EXPRESSION ]);


This pragma is identical in syntax and semantics to pragma
``Export`` as defined in the Ada Reference Manual.  It is
provided for compatibility with some Ada 83 compilers that
used this pragma for exactly the same purposes as pragma
``Export`` before the latter was standardized.

Pragma External_Name_Casing
===========================
.. index:: Dec Ada 83 casing compatibility

.. index:: External Names, casing

.. index:: Casing of External names


Syntax:


::

  pragma External_Name_Casing (
    Uppercase | Lowercase
    [, Uppercase | Lowercase | As_Is]);


This pragma provides control over the casing of external names associated
with Import and Export pragmas.  There are two cases to consider:



* Implicit external names

  Implicit external names are derived from identifiers.  The most common case
  arises when a standard Ada Import or Export pragma is used with only two
  arguments, as in:

  .. code-block:: ada

       pragma Import (C, C_Routine);

  Since Ada is a case-insensitive language, the spelling of the identifier in
  the Ada source program does not provide any information on the desired
  casing of the external name, and so a convention is needed.  In GNAT the
  default treatment is that such names are converted to all lower case
  letters.  This corresponds to the normal C style in many environments.
  The first argument of pragma ``External_Name_Casing`` can be used to
  control this treatment.  If ``Uppercase`` is specified, then the name
  will be forced to all uppercase letters.  If ``Lowercase`` is specified,
  then the normal default of all lower case letters will be used.

  This same implicit treatment is also used in the case of extended DEC Ada 83
  compatible Import and Export pragmas where an external name is explicitly
  specified using an identifier rather than a string.


* Explicit external names

  Explicit external names are given as string literals.  The most common case
  arises when a standard Ada Import or Export pragma is used with three
  arguments, as in:

  .. code-block:: ada

    pragma Import (C, C_Routine, "C_routine");

  In this case, the string literal normally provides the exact casing required
  for the external name.  The second argument of pragma
  ``External_Name_Casing`` may be used to modify this behavior.
  If ``Uppercase`` is specified, then the name
  will be forced to all uppercase letters.  If ``Lowercase`` is specified,
  then the name will be forced to all lowercase letters.  A specification of
  ``As_Is`` provides the normal default behavior in which the casing is
  taken from the string provided.

This pragma may appear anywhere that a pragma is valid. In particular, it
can be used as a configuration pragma in the :file:`gnat.adc` file, in which
case it applies to all subsequent compilations, or it can be used as a program
unit pragma, in which case it only applies to the current unit, or it can
be used more locally to control individual Import/Export pragmas.

It was primarily intended for use with OpenVMS systems, where many
compilers convert all symbols to upper case by default.  For interfacing to
such compilers (e.g., the DEC C compiler), it may be convenient to use
the pragma:

.. code-block:: ada

  pragma External_Name_Casing (Uppercase, Uppercase);


to enforce the upper casing of all external symbols.

Pragma Fast_Math
================

Syntax:


.. code-block:: ada

  pragma Fast_Math;


This is a configuration pragma which activates a mode in which speed is
considered more important for floating-point operations than absolutely
accurate adherence to the requirements of the standard. Currently the
following operations are affected:



*Complex Multiplication*
  The normal simple formula for complex multiplication can result in intermediate
  overflows for numbers near the end of the range. The Ada standard requires that
  this situation be detected and corrected by scaling, but in Fast_Math mode such
  cases will simply result in overflow. Note that to take advantage of this you
  must instantiate your own version of ``Ada.Numerics.Generic_Complex_Types``
  under control of the pragma, rather than use the preinstantiated versions.

.. _Pragma-Favor_Top_Level:

Pragma Favor_Top_Level
======================

Syntax:


.. code-block:: ada

  pragma Favor_Top_Level (type_NAME);


The argument of pragma ``Favor_Top_Level`` must be a named access-to-subprogram
type. This pragma is an efficiency hint to the compiler, regarding the use of
``'Access`` or ``'Unrestricted_Access`` on nested (non-library-level) subprograms.
The pragma means that nested subprograms are not used with this type, or are
rare, so that the generated code should be efficient in the top-level case.
When this pragma is used, dynamically generated trampolines may be used on some
targets for nested subprograms. See restriction ``No_Implicit_Dynamic_Code``.

Pragma Finalize_Storage_Only
============================

Syntax:


.. code-block:: ada

  pragma Finalize_Storage_Only (first_subtype_LOCAL_NAME);


The argument of pragma ``Finalize_Storage_Only`` must denote a local type which
is derived from ``Ada.Finalization.Controlled`` or ``Limited_Controlled``. The
pragma suppresses the call to ``Finalize`` for declared library-level objects
of the argument type. This is mostly useful for types where finalization is
only used to deal with storage reclamation since in most environments it is
not necessary to reclaim memory just before terminating execution, hence the
name. Note that this pragma does not suppress Finalize calls for library-level
heap-allocated objects (see pragma ``No_Heap_Finalization``).

Pragma Float_Representation
===========================

Syntax::

  pragma Float_Representation (FLOAT_REP[, float_type_LOCAL_NAME]);

  FLOAT_REP ::= VAX_Float | IEEE_Float


In the one argument form, this pragma is a configuration pragma which
allows control over the internal representation chosen for the predefined
floating point types declared in the packages ``Standard`` and
``System``. This pragma is only provided for compatibility and has no effect.

The two argument form specifies the representation to be used for
the specified floating-point type. The argument must
be ``IEEE_Float`` to specify the use of IEEE format, as follows:

*
  For a digits value of 6, 32-bit IEEE short format will be used.
*
  For a digits value of 15, 64-bit IEEE long format will be used.
*
  No other value of digits is permitted.

.. _Pragma-Ghost:

Pragma Ghost
============

Syntax:

.. code-block:: ada

  pragma Ghost [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect ``Ghost`` in the SPARK
2014 Reference Manual, section 6.9.

.. _Pragma-Global:

Pragma Global
=============

Syntax:

.. code-block:: ada

  pragma Global (GLOBAL_SPECIFICATION);

  GLOBAL_SPECIFICATION ::=
       null
    | (GLOBAL_LIST)
    | (MODED_GLOBAL_LIST {, MODED_GLOBAL_LIST})

  MODED_GLOBAL_LIST ::= MODE_SELECTOR => GLOBAL_LIST

  MODE_SELECTOR ::= In_Out | Input | Output | Proof_In
  GLOBAL_LIST   ::= GLOBAL_ITEM | (GLOBAL_ITEM {, GLOBAL_ITEM})
  GLOBAL_ITEM   ::= NAME

For the semantics of this pragma, see the entry for aspect ``Global`` in the
SPARK 2014 Reference Manual, section 6.1.4.

Pragma Ident
============

Syntax:


.. code-block:: ada

  pragma Ident (static_string_EXPRESSION);


This pragma is identical in effect to pragma ``Comment``. It is provided
for compatibility with other Ada compilers providing this pragma.

Pragma Ignore_Pragma
====================

Syntax:


.. code-block:: ada

  pragma Ignore_Pragma (pragma_IDENTIFIER);

This is a configuration pragma
that takes a single argument that is a simple identifier. Any subsequent
use of a pragma whose pragma identifier matches this argument will be
silently ignored. This may be useful when legacy code or code intended
for compilation with some other compiler contains pragmas that match the
name, but not the exact implementation, of a GNAT pragma. The use of this
pragma allows such pragmas to be ignored, which may be useful in CodePeer
mode, or during porting of legacy code.

Pragma Implementation_Defined
=============================

Syntax:


.. code-block:: ada

  pragma Implementation_Defined (local_NAME);


This pragma marks a previously declared entity as implementation-defined.
For an overloaded entity, applies to the most recent homonym.


.. code-block:: ada

  pragma Implementation_Defined;


The form with no arguments appears anywhere within a scope, most
typically a package spec, and indicates that all entities that are
defined within the package spec are Implementation_Defined.

This pragma is used within the GNAT runtime library to identify
implementation-defined entities introduced in language-defined units,
for the purpose of implementing the No_Implementation_Identifiers
restriction.

Pragma Implemented
==================

Syntax:


::

  pragma Implemented (procedure_LOCAL_NAME, implementation_kind);

  implementation_kind ::= By_Entry | By_Protected_Procedure | By_Any


This is an Ada 2012 representation pragma which applies to protected, task
and synchronized interface primitives. The use of pragma Implemented provides
a way to impose a static requirement on the overriding operation by adhering
to one of the three implementation kinds: entry, protected procedure or any of
the above. This pragma is available in all earlier versions of Ada as an
implementation-defined pragma.


.. code-block:: ada

  type Synch_Iface is synchronized interface;
  procedure Prim_Op (Obj : in out Iface) is abstract;
  pragma Implemented (Prim_Op, By_Protected_Procedure);

  protected type Prot_1 is new Synch_Iface with
     procedure Prim_Op;  --  Legal
  end Prot_1;

  protected type Prot_2 is new Synch_Iface with
     entry Prim_Op;      --  Illegal
  end Prot_2;

  task type Task_Typ is new Synch_Iface with
     entry Prim_Op;      --  Illegal
  end Task_Typ;


When applied to the procedure_or_entry_NAME of a requeue statement, pragma
Implemented determines the runtime behavior of the requeue. Implementation kind
By_Entry guarantees that the action of requeueing will proceed from an entry to
another entry. Implementation kind By_Protected_Procedure transforms the
requeue into a dispatching call, thus eliminating the chance of blocking. Kind
By_Any shares the behavior of By_Entry and By_Protected_Procedure depending on
the target's overriding subprogram kind.

Pragma Implicit_Packing
=======================
.. index:: Rational Profile

Syntax:


.. code-block:: ada

  pragma Implicit_Packing;


This is a configuration pragma that requests implicit packing for packed
arrays for which a size clause is given but no explicit pragma Pack or
specification of Component_Size is present. It also applies to records
where no record representation clause is present. Consider this example:


.. code-block:: ada

  type R is array (0 .. 7) of Boolean;
  for R'Size use 8;


In accordance with the recommendation in the RM (RM 13.3(53)), a Size clause
does not change the layout of a composite object. So the Size clause in the
above example is normally rejected, since the default layout of the array uses
8-bit components, and thus the array requires a minimum of 64 bits.

If this declaration is compiled in a region of code covered by an occurrence
of the configuration pragma Implicit_Packing, then the Size clause in this
and similar examples will cause implicit packing and thus be accepted. For
this implicit packing to occur, the type in question must be an array of small
components whose size is known at compile time, and the Size clause must
specify the exact size that corresponds to the number of elements in the array
multiplied by the size in bits of the component type (both single and
multi-dimensioned arrays can be controlled with this pragma).

.. index:: Array packing

Similarly, the following example shows the use in the record case


.. code-block:: ada

  type r is record
     a, b, c, d, e, f, g, h : boolean;
     chr                    : character;
  end record;
  for r'size use 16;


Without a pragma Pack, each Boolean field requires 8 bits, so the
minimum size is 72 bits, but with a pragma Pack, 16 bits would be
sufficient. The use of pragma Implicit_Packing allows this record
declaration to compile without an explicit pragma Pack.

Pragma Import_Function
======================

Syntax:


::

  pragma Import_Function (
       [Internal                 =>] LOCAL_NAME,
    [, [External                 =>] EXTERNAL_SYMBOL]
    [, [Parameter_Types          =>] PARAMETER_TYPES]
    [, [Result_Type              =>] SUBTYPE_MARK]
    [, [Mechanism                =>] MECHANISM]
    [, [Result_Mechanism         =>] MECHANISM_NAME]);

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION

  PARAMETER_TYPES ::=
    null
  | TYPE_DESIGNATOR {, TYPE_DESIGNATOR}

  TYPE_DESIGNATOR ::=
    subtype_NAME
  | subtype_Name ' Access

  MECHANISM ::=
    MECHANISM_NAME
  | (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})

  MECHANISM_ASSOCIATION ::=
    [formal_parameter_NAME =>] MECHANISM_NAME

  MECHANISM_NAME ::=
    Value
  | Reference


This pragma is used in conjunction with a pragma ``Import`` to
specify additional information for an imported function.  The pragma
``Import`` (or equivalent pragma ``Interface``) must precede the
``Import_Function`` pragma and both must appear in the same
declarative part as the function specification.

The ``Internal`` argument must uniquely designate
the function to which the
pragma applies.  If more than one function name exists of this name in
the declarative part you must use the ``Parameter_Types`` and
``Result_Type`` parameters to achieve the required unique
designation.  Subtype marks in these parameters must exactly match the
subtypes in the corresponding function specification, using positional
notation to match parameters with subtype marks.
The form with an ``'Access`` attribute can be used to match an
anonymous access parameter.

You may optionally use the ``Mechanism`` and ``Result_Mechanism``
parameters to specify passing mechanisms for the
parameters and result.  If you specify a single mechanism name, it
applies to all parameters.  Otherwise you may specify a mechanism on a
parameter by parameter basis using either positional or named
notation.  If the mechanism is not specified, the default mechanism
is used.

Pragma Import_Object
====================

Syntax:


::

  pragma Import_Object
       [Internal =>] LOCAL_NAME
    [, [External =>] EXTERNAL_SYMBOL]
    [, [Size     =>] EXTERNAL_SYMBOL]);

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION


This pragma designates an object as imported, and apart from the
extended rules for external symbols, is identical in effect to the use of
the normal ``Import`` pragma applied to an object.  Unlike the
subprogram case, you need not use a separate ``Import`` pragma,
although you may do so (and probably should do so from a portability
point of view).  ``size`` is syntax checked, but otherwise ignored by
GNAT.

Pragma Import_Procedure
=======================

Syntax:


::

  pragma Import_Procedure (
       [Internal                 =>] LOCAL_NAME
    [, [External                 =>] EXTERNAL_SYMBOL]
    [, [Parameter_Types          =>] PARAMETER_TYPES]
    [, [Mechanism                =>] MECHANISM]);

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION

  PARAMETER_TYPES ::=
    null
  | TYPE_DESIGNATOR {, TYPE_DESIGNATOR}

  TYPE_DESIGNATOR ::=
    subtype_NAME
  | subtype_Name ' Access

  MECHANISM ::=
    MECHANISM_NAME
  | (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})

  MECHANISM_ASSOCIATION ::=
    [formal_parameter_NAME =>] MECHANISM_NAME

  MECHANISM_NAME ::= Value | Reference


This pragma is identical to ``Import_Function`` except that it
applies to a procedure rather than a function and the parameters
``Result_Type`` and ``Result_Mechanism`` are not permitted.

Pragma Import_Valued_Procedure
==============================

Syntax:


::

  pragma Import_Valued_Procedure (
       [Internal                 =>] LOCAL_NAME
    [, [External                 =>] EXTERNAL_SYMBOL]
    [, [Parameter_Types          =>] PARAMETER_TYPES]
    [, [Mechanism                =>] MECHANISM]);

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION

  PARAMETER_TYPES ::=
    null
  | TYPE_DESIGNATOR {, TYPE_DESIGNATOR}

  TYPE_DESIGNATOR ::=
    subtype_NAME
  | subtype_Name ' Access

  MECHANISM ::=
    MECHANISM_NAME
  | (MECHANISM_ASSOCIATION {, MECHANISM_ASSOCIATION})

  MECHANISM_ASSOCIATION ::=
    [formal_parameter_NAME =>] MECHANISM_NAME

  MECHANISM_NAME ::= Value | Reference


This pragma is identical to ``Import_Procedure`` except that the
first parameter of ``LOCAL_NAME``, which must be present, must be of
mode ``out``, and externally the subprogram is treated as a function
with this parameter as the result of the function.  The purpose of this
capability is to allow the use of ``out`` and ``in out``
parameters in interfacing to external functions (which are not permitted
in Ada functions).  You may optionally use the ``Mechanism``
parameters to specify passing mechanisms for the parameters.
If you specify a single mechanism name, it applies to all parameters.
Otherwise you may specify a mechanism on a parameter by parameter
basis using either positional or named notation.  If the mechanism is not
specified, the default mechanism is used.

Note that it is important to use this pragma in conjunction with a separate
pragma Import that specifies the desired convention, since otherwise the
default convention is Ada, which is almost certainly not what is required.

Pragma Independent
==================

Syntax:


.. code-block:: ada

  pragma Independent (Local_NAME);


This pragma is standard in Ada 2012 mode (which also provides an aspect
of the same name). It is also available as an implementation-defined
pragma in all earlier versions. It specifies that the
designated object or all objects of the designated type must be
independently addressable. This means that separate tasks can safely
manipulate such objects. For example, if two components of a record are
independent, then two separate tasks may access these two components.
This may place
constraints on the representation of the object (for instance prohibiting
tight packing).

Pragma Independent_Components
=============================

Syntax:


.. code-block:: ada

  pragma Independent_Components (Local_NAME);


This pragma is standard in Ada 2012 mode (which also provides an aspect
of the same name). It is also available as an implementation-defined
pragma in all earlier versions. It specifies that the components of the
designated object, or the components of each object of the designated
type, must be
independently addressable. This means that separate tasks can safely
manipulate separate components in the composite object. This may place
constraints on the representation of the object (for instance prohibiting
tight packing).

.. _Pragma-Initial_Condition:

Pragma Initial_Condition
========================

Syntax:

.. code-block:: ada

  pragma Initial_Condition (boolean_EXPRESSION);

For the semantics of this pragma, see the entry for aspect ``Initial_Condition``
in the SPARK 2014 Reference Manual, section 7.1.6.

Pragma Initialize_Scalars
=========================
.. index:: debugging with Initialize_Scalars

Syntax:


.. code-block:: ada

  pragma Initialize_Scalars
    [ ( TYPE_VALUE_PAIR {, TYPE_VALUE_PAIR} ) ];

  TYPE_VALUE_PAIR ::=
    SCALAR_TYPE => static_EXPRESSION

  SCALAR_TYPE :=
    Short_Float
  | Float
  | Long_Float
  | Long_Long_Flat
  | Signed_8
  | Signed_16
  | Signed_32
  | Signed_64
  | Unsigned_8
  | Unsigned_16
  | Unsigned_32
  | Unsigned_64


This pragma is similar to ``Normalize_Scalars`` conceptually but has two
important differences.

First, there is no requirement for the pragma to be used uniformly in all units
of a partition. In particular, it is fine to use this just for some or all of
the application units of a partition, without needing to recompile the run-time
library. In the case where some units are compiled with the pragma, and some
without, then a declaration of a variable where the type is defined in package
Standard or is locally declared will always be subject to initialization, as
will any declaration of a scalar variable. For composite variables, whether the
variable is initialized may also depend on whether the package in which the
type of the variable is declared is compiled with the pragma.

The other important difference is that the programmer can control the value
used for initializing scalar objects. This effect can be achieved in several
different ways:

* At compile time, the programmer can specify the invalid value for a
  particular family of scalar types using the optional arguments of the pragma.

  The compile-time approach is intended to optimize the generated code for the
  pragma, by possibly using fast operations such as ``memset``. Note that such
  optimizations require using values where the bytes all have the same binary
  representation.

* At bind time, the programmer has several options:

  * Initialization with invalid values (similar to Normalize_Scalars, though
    for Initialize_Scalars it is not always possible to determine the invalid
    values in complex cases like signed component fields with nonstandard
    sizes).

  * Initialization with high values.

  * Initialization with low values.

  * Initialization with a specific bit pattern.

  See the GNAT User's Guide for binder options for specifying these cases.

  The bind-time approach is intended to provide fast turnaround for testing
  with different values, without having to recompile the program.

* At execution time, the programmer can specify the invalid values using an
  environment variable. See the GNAT User's Guide for details.

  The execution-time approach is intended to provide fast turnaround for
  testing with different values, without having to recompile and rebind the
  program.

Note that pragma ``Initialize_Scalars`` is particularly useful in conjunction
with the enhanced validity checking that is now provided in GNAT, which checks
for invalid values under more conditions. Using this feature (see description
of the *-gnatV* flag in the GNAT User's Guide) in conjunction with pragma
``Initialize_Scalars`` provides a powerful new tool to assist in the detection
of problems caused by uninitialized variables.

Note: the use of ``Initialize_Scalars`` has a fairly extensive effect on the
generated code. This may cause your code to be substantially larger. It may
also cause an increase in the amount of stack required, so it is probably a
good idea to turn on stack checking (see description of stack checking in the
GNAT User's Guide) when using this pragma.

.. _Pragma-Initializes:

Pragma Initializes
==================

Syntax:

.. code-block:: ada

  pragma Initializes (INITIALIZATION_LIST);

  INITIALIZATION_LIST ::=
       null
    | (INITIALIZATION_ITEM {, INITIALIZATION_ITEM})

  INITIALIZATION_ITEM ::= name [=> INPUT_LIST]

  INPUT_LIST ::=
       null
    |  INPUT
    | (INPUT {, INPUT})

  INPUT ::= name

For the semantics of this pragma, see the entry for aspect ``Initializes`` in the
SPARK 2014 Reference Manual, section 7.1.5.

.. _Pragma-Inline_Always:

Pragma Inline_Always
====================

Syntax:


::

  pragma Inline_Always (NAME [, NAME]);


Similar to pragma ``Inline`` except that inlining is unconditional.
Inline_Always instructs the compiler to inline every direct call to the
subprogram or else to emit a compilation error, independently of any
option, in particular *-gnatn* or *-gnatN* or the optimization level.
It is an error to take the address or access of ``NAME``. It is also an error to
apply this pragma to a primitive operation of a tagged type. Thanks to such
restrictions, the compiler is allowed to remove the out-of-line body of ``NAME``.

Pragma Inline_Generic
=====================

Syntax:


::

  pragma Inline_Generic (GNAME {, GNAME});

  GNAME ::= generic_unit_NAME | generic_instance_NAME


This pragma is provided for compatibility with Dec Ada 83. It has
no effect in GNAT (which always inlines generics), other
than to check that the given names are all names of generic units or
generic instances.

Pragma Interface
================

Syntax:


::

  pragma Interface (
       [Convention    =>] convention_identifier,
       [Entity        =>] local_NAME
    [, [External_Name =>] static_string_expression]
    [, [Link_Name     =>] static_string_expression]);


This pragma is identical in syntax and semantics to
the standard Ada pragma ``Import``.  It is provided for compatibility
with Ada 83.  The definition is upwards compatible both with pragma
``Interface`` as defined in the Ada 83 Reference Manual, and also
with some extended implementations of this pragma in certain Ada 83
implementations.  The only difference between pragma ``Interface``
and pragma ``Import`` is that there is special circuitry to allow
both pragmas to appear for the same subprogram entity (normally it
is illegal to have multiple ``Import`` pragmas. This is useful in
maintaining Ada 83/Ada 95 compatibility and is compatible with other
Ada 83 compilers.

Pragma Interface_Name
=====================

Syntax:


::

  pragma Interface_Name (
       [Entity        =>] LOCAL_NAME
    [, [External_Name =>] static_string_EXPRESSION]
    [, [Link_Name     =>] static_string_EXPRESSION]);


This pragma provides an alternative way of specifying the interface name
for an interfaced subprogram, and is provided for compatibility with Ada
83 compilers that use the pragma for this purpose.  You must provide at
least one of ``External_Name`` or ``Link_Name``.

Pragma Interrupt_Handler
========================

Syntax:


.. code-block:: ada

  pragma Interrupt_Handler (procedure_LOCAL_NAME);


This program unit pragma is supported for parameterless protected procedures
as described in Annex C of the Ada Reference Manual.

Pragma Interrupt_State
======================

Syntax:


::

  pragma Interrupt_State
   ([Name  =>] value,
    [State =>] SYSTEM | RUNTIME | USER);


Normally certain interrupts are reserved to the implementation.  Any attempt
to attach an interrupt causes Program_Error to be raised, as described in
RM C.3.2(22).  A typical example is the ``SIGINT`` interrupt used in
many systems for an :kbd:`Ctrl-C` interrupt.  Normally this interrupt is
reserved to the implementation, so that :kbd:`Ctrl-C` can be used to
interrupt execution.  Additionally, signals such as ``SIGSEGV``,
``SIGABRT``, ``SIGFPE`` and ``SIGILL`` are often mapped to specific
Ada exceptions, or used to implement run-time functions such as the
``abort`` statement and stack overflow checking.

Pragma ``Interrupt_State`` provides a general mechanism for overriding
such uses of interrupts.  It subsumes the functionality of pragma
``Unreserve_All_Interrupts``.  Pragma ``Interrupt_State`` is not
available on Windows.  On all other platforms than VxWorks,
it applies to signals; on VxWorks, it applies to vectored hardware interrupts
and may be used to mark interrupts required by the board support package
as reserved.

Interrupts can be in one of three states:

* System

  The interrupt is reserved (no Ada handler can be installed), and the
  Ada run-time may not install a handler. As a result you are guaranteed
  standard system default action if this interrupt is raised. This also allows
  installing a low level handler via C APIs such as sigaction(), outside
  of Ada control.

* Runtime

  The interrupt is reserved (no Ada handler can be installed). The run time
  is allowed to install a handler for internal control purposes, but is
  not required to do so.

* User

  The interrupt is unreserved.  The user may install an Ada handler via
  Ada.Interrupts and pragma Interrupt_Handler or Attach_Handler to provide
  some other action.

These states are the allowed values of the ``State`` parameter of the
pragma.  The ``Name`` parameter is a value of the type
``Ada.Interrupts.Interrupt_ID``.  Typically, it is a name declared in
``Ada.Interrupts.Names``.

This is a configuration pragma, and the binder will check that there
are no inconsistencies between different units in a partition in how a
given interrupt is specified. It may appear anywhere a pragma is legal.

The effect is to move the interrupt to the specified state.

By declaring interrupts to be SYSTEM, you guarantee the standard system
action, such as a core dump.

By declaring interrupts to be USER, you guarantee that you can install
a handler.

Note that certain signals on many operating systems cannot be caught and
handled by applications.  In such cases, the pragma is ignored.  See the
operating system documentation, or the value of the array ``Reserved``
declared in the spec of package ``System.OS_Interface``.

Overriding the default state of signals used by the Ada runtime may interfere
with an application's runtime behavior in the cases of the synchronous signals,
and in the case of the signal used to implement the ``abort`` statement.

.. _Pragma-Invariant:

Pragma Invariant
================

Syntax:


::

  pragma Invariant
    ([Entity =>]    private_type_LOCAL_NAME,
     [Check  =>]    EXPRESSION
     [,[Message =>] String_Expression]);


This pragma provides exactly the same capabilities as the Type_Invariant aspect
defined in AI05-0146-1, and in the Ada 2012 Reference Manual. The
Type_Invariant aspect is fully implemented in Ada 2012 mode, but since it
requires the use of the aspect syntax, which is not available except in 2012
mode, it is not possible to use the Type_Invariant aspect in earlier versions
of Ada. However the Invariant pragma may be used in any version of Ada. Also
note that the aspect Invariant is a synonym in GNAT for the aspect
Type_Invariant, but there is no pragma Type_Invariant.

The pragma must appear within the visible part of the package specification,
after the type to which its Entity argument appears. As with the Invariant
aspect, the Check expression is not analyzed until the end of the visible
part of the package, so it may contain forward references. The Message
argument, if present, provides the exception message used if the invariant
is violated. If no Message parameter is provided, a default message that
identifies the line on which the pragma appears is used.

It is permissible to have multiple Invariants for the same type entity, in
which case they are and'ed together. It is permissible to use this pragma
in Ada 2012 mode, but you cannot have both an invariant aspect and an
invariant pragma for the same entity.

For further details on the use of this pragma, see the Ada 2012 documentation
of the Type_Invariant aspect.

Pragma Keep_Names
=================

Syntax:


::

  pragma Keep_Names ([On =>] enumeration_first_subtype_LOCAL_NAME);


The ``LOCAL_NAME`` argument
must refer to an enumeration first subtype
in the current declarative part. The effect is to retain the enumeration
literal names for use by ``Image`` and ``Value`` even if a global
``Discard_Names`` pragma applies. This is useful when you want to
generally suppress enumeration literal names and for example you therefore
use a ``Discard_Names`` pragma in the :file:`gnat.adc` file, but you
want to retain the names for specific enumeration types.

Pragma License
==============
.. index:: License checking

Syntax:


.. code-block:: ada

  pragma License (Unrestricted | GPL | Modified_GPL | Restricted);


This pragma is provided to allow automated checking for appropriate license
conditions with respect to the standard and modified GPL.  A pragma
``License``, which is a configuration pragma that typically appears at
the start of a source file or in a separate :file:`gnat.adc` file, specifies
the licensing conditions of a unit as follows:

* Unrestricted
  This is used for a unit that can be freely used with no license restrictions.
  Examples of such units are public domain units, and units from the Ada
  Reference Manual.

* GPL
  This is used for a unit that is licensed under the unmodified GPL, and which
  therefore cannot be ``with``\ ed by a restricted unit.

* Modified_GPL
  This is used for a unit licensed under the GNAT modified GPL that includes
  a special exception paragraph that specifically permits the inclusion of
  the unit in programs without requiring the entire program to be released
  under the GPL.

* Restricted
  This is used for a unit that is restricted in that it is not permitted to
  depend on units that are licensed under the GPL.  Typical examples are
  proprietary code that is to be released under more restrictive license
  conditions.  Note that restricted units are permitted to ``with`` units
  which are licensed under the modified GPL (this is the whole point of the
  modified GPL).


Normally a unit with no ``License`` pragma is considered to have an
unknown license, and no checking is done.  However, standard GNAT headers
are recognized, and license information is derived from them as follows.

A GNAT license header starts with a line containing 78 hyphens.  The following
comment text is searched for the appearance of any of the following strings.

If the string 'GNU General Public License' is found, then the unit is assumed
to have GPL license, unless the string 'As a special exception' follows, in
which case the license is assumed to be modified GPL.

If one of the strings
'This specification is adapted from the Ada Semantic Interface' or
'This specification is derived from the Ada Reference Manual' is found
then the unit is assumed to be unrestricted.

These default actions means that a program with a restricted license pragma
will automatically get warnings if a GPL unit is inappropriately
``with``\ ed.  For example, the program:

.. code-block:: ada

  with Sem_Ch3;
  with GNAT.Sockets;
  procedure Secret_Stuff is
    ...
  end Secret_Stuff


if compiled with pragma ``License`` (``Restricted``) in a
:file:`gnat.adc` file will generate the warning::

  1.  with Sem_Ch3;
          |
     >>> license of withed unit "Sem_Ch3" is incompatible

  2.  with GNAT.Sockets;
  3.  procedure Secret_Stuff is


Here we get a warning on ``Sem_Ch3`` since it is part of the GNAT
compiler and is licensed under the
GPL, but no warning for ``GNAT.Sockets`` which is part of the GNAT
run time, and is therefore licensed under the modified GPL.

Pragma Link_With
================

Syntax:


::

  pragma Link_With (static_string_EXPRESSION {,static_string_EXPRESSION});


This pragma is provided for compatibility with certain Ada 83 compilers.
It has exactly the same effect as pragma ``Linker_Options`` except
that spaces occurring within one of the string expressions are treated
as separators. For example, in the following case:

.. code-block:: ada

  pragma Link_With ("-labc -ldef");


results in passing the strings ``-labc`` and ``-ldef`` as two
separate arguments to the linker. In addition pragma Link_With allows
multiple arguments, with the same effect as successive pragmas.

Pragma Linker_Alias
===================

Syntax:


::

  pragma Linker_Alias (
    [Entity =>] LOCAL_NAME,
    [Target =>] static_string_EXPRESSION);


``LOCAL_NAME`` must refer to an object that is declared at the library
level. This pragma establishes the given entity as a linker alias for the
given target. It is equivalent to ``__attribute__((alias))`` in GNU C
and causes ``LOCAL_NAME`` to be emitted as an alias for the symbol
``static_string_EXPRESSION`` in the object file, that is to say no space
is reserved for ``LOCAL_NAME`` by the assembler and it will be resolved
to the same address as ``static_string_EXPRESSION`` by the linker.

The actual linker name for the target must be used (e.g., the fully
encoded name with qualification in Ada, or the mangled name in C++),
or it must be declared using the C convention with ``pragma Import``
or ``pragma Export``.

Not all target machines support this pragma. On some of them it is accepted
only if ``pragma Weak_External`` has been applied to ``LOCAL_NAME``.


.. code-block:: ada

  --  Example of the use of pragma Linker_Alias

  package p is
    i : Integer := 1;
    pragma Export (C, i);

    new_name_for_i : Integer;
    pragma Linker_Alias (new_name_for_i, "i");
  end p;


Pragma Linker_Constructor
=========================

Syntax:


.. code-block:: ada

  pragma Linker_Constructor (procedure_LOCAL_NAME);


``procedure_LOCAL_NAME`` must refer to a parameterless procedure that
is declared at the library level. A procedure to which this pragma is
applied will be treated as an initialization routine by the linker.
It is equivalent to ``__attribute__((constructor))`` in GNU C and
causes ``procedure_LOCAL_NAME`` to be invoked before the entry point
of the executable is called (or immediately after the shared library is
loaded if the procedure is linked in a shared library), in particular
before the Ada run-time environment is set up.

Because of these specific contexts, the set of operations such a procedure
can perform is very limited and the type of objects it can manipulate is
essentially restricted to the elementary types. In particular, it must only
contain code to which pragma Restrictions (No_Elaboration_Code) applies.

This pragma is used by GNAT to implement auto-initialization of shared Stand
Alone Libraries, which provides a related capability without the restrictions
listed above. Where possible, the use of Stand Alone Libraries is preferable
to the use of this pragma.

Pragma Linker_Destructor
========================

Syntax:


.. code-block:: ada

  pragma Linker_Destructor (procedure_LOCAL_NAME);


``procedure_LOCAL_NAME`` must refer to a parameterless procedure that
is declared at the library level. A procedure to which this pragma is
applied will be treated as a finalization routine by the linker.
It is equivalent to ``__attribute__((destructor))`` in GNU C and
causes ``procedure_LOCAL_NAME`` to be invoked after the entry point
of the executable has exited (or immediately before the shared library
is unloaded if the procedure is linked in a shared library), in particular
after the Ada run-time environment is shut down.

See ``pragma Linker_Constructor`` for the set of restrictions that apply
because of these specific contexts.

.. _Pragma-Linker_Section:

Pragma Linker_Section
=====================

Syntax:


::

  pragma Linker_Section (
    [Entity  =>] LOCAL_NAME,
    [Section =>] static_string_EXPRESSION);


``LOCAL_NAME`` must refer to an object, type, or subprogram that is
declared at the library level. This pragma specifies the name of the
linker section for the given entity. It is equivalent to
``__attribute__((section))`` in GNU C and causes ``LOCAL_NAME`` to
be placed in the ``static_string_EXPRESSION`` section of the
executable (assuming the linker doesn't rename the section).
GNAT also provides an implementation defined aspect of the same name.

In the case of specifying this aspect for a type, the effect is to
specify the corresponding section for all library-level objects of
the type that do not have an explicit linker section set. Note that
this only applies to whole objects, not to components of composite objects.

In the case of a subprogram, the linker section applies to all previously
declared matching overloaded subprograms in the current declarative part
which do not already have a linker section assigned. The linker section
aspect is useful in this case for specifying different linker sections
for different elements of such an overloaded set.

Note that an empty string specifies that no linker section is specified.
This is not quite the same as omitting the pragma or aspect, since it
can be used to specify that one element of an overloaded set of subprograms
has the default linker section, or that one object of a type for which a
linker section is specified should has the default linker section.

The compiler normally places library-level entities in standard sections
depending on the class: procedures and functions generally go in the
``.text`` section, initialized variables in the ``.data`` section
and uninitialized variables in the ``.bss`` section.

Other, special sections may exist on given target machines to map special
hardware, for example I/O ports or flash memory. This pragma is a means to
defer the final layout of the executable to the linker, thus fully working
at the symbolic level with the compiler.

Some file formats do not support arbitrary sections so not all target
machines support this pragma. The use of this pragma may cause a program
execution to be erroneous if it is used to place an entity into an
inappropriate section (e.g., a modified variable into the ``.text``
section). See also ``pragma Persistent_BSS``.


.. code-block:: ada

  --  Example of the use of pragma Linker_Section

  package IO_Card is
    Port_A : Integer;
    pragma Volatile (Port_A);
    pragma Linker_Section (Port_A, ".bss.port_a");

    Port_B : Integer;
    pragma Volatile (Port_B);
    pragma Linker_Section (Port_B, ".bss.port_b");

    type Port_Type is new Integer with Linker_Section => ".bss";
    PA : Port_Type with Linker_Section => ".bss.PA";
    PB : Port_Type; --  ends up in linker section ".bss"

    procedure Q with Linker_Section => "Qsection";
  end IO_Card;

.. _Pragma-Lock_Free:

Pragma Lock_Free
================

Syntax:
This pragma may be specified for protected types or objects. It specifies that
the implementation of protected operations must be implemented without locks.
Compilation fails if the compiler cannot generate lock-free code for the
operations.

The current conditions required to support this pragma are:

* Protected type declarations may not contain entries
* Protected subprogram declarations may not have nonelementary parameters

In addition, each protected subprogram body must satisfy:

* May reference only one protected component
* May not reference nonconstant entities outside the protected subprogram
  scope.
* May not contain address representation items, allocators, or quantified
  expressions.
* May not contain delay, goto, loop, or procedure-call statements.
* May not contain exported and imported entities
* May not dereferenced access values
* Function calls and attribute references must be static


Pragma Loop_Invariant
=====================

Syntax:


.. code-block:: ada

  pragma Loop_Invariant ( boolean_EXPRESSION );


The effect of this pragma is similar to that of pragma ``Assert``,
except that in an ``Assertion_Policy`` pragma, the identifier
``Loop_Invariant`` is used to control whether it is ignored or checked
(or disabled).

``Loop_Invariant`` can only appear as one of the items in the sequence
of statements of a loop body, or nested inside block statements that
appear in the sequence of statements of a loop body.
The intention is that it be used to
represent a "loop invariant" assertion, i.e. something that is true each
time through the loop, and which can be used to show that the loop is
achieving its purpose.

Multiple ``Loop_Invariant`` and ``Loop_Variant`` pragmas that
apply to the same loop should be grouped in the same sequence of
statements.

To aid in writing such invariants, the special attribute ``Loop_Entry``
may be used to refer to the value of an expression on entry to the loop. This
attribute can only be used within the expression of a ``Loop_Invariant``
pragma. For full details, see documentation of attribute ``Loop_Entry``.

Pragma Loop_Optimize
====================

Syntax:


::

  pragma Loop_Optimize (OPTIMIZATION_HINT {, OPTIMIZATION_HINT});

  OPTIMIZATION_HINT ::= Ivdep | No_Unroll | Unroll | No_Vector | Vector


This pragma must appear immediately within a loop statement.  It allows the
programmer to specify optimization hints for the enclosing loop.  The hints
are not mutually exclusive and can be freely mixed, but not all combinations
will yield a sensible outcome.

There are five supported optimization hints for a loop:

* Ivdep

  The programmer asserts that there are no loop-carried dependencies
  which would prevent consecutive iterations of the loop from being
  executed simultaneously.

* No_Unroll

  The loop must not be unrolled.  This is a strong hint: the compiler will not
  unroll a loop marked with this hint.

* Unroll

  The loop should be unrolled.  This is a weak hint: the compiler will try to
  apply unrolling to this loop preferably to other optimizations, notably
  vectorization, but there is no guarantee that the loop will be unrolled.

* No_Vector

  The loop must not be vectorized.  This is a strong hint: the compiler will not
  vectorize a loop marked with this hint.

* Vector

  The loop should be vectorized.  This is a weak hint: the compiler will try to
  apply vectorization to this loop preferably to other optimizations, notably
  unrolling, but there is no guarantee that the loop will be vectorized.


These hints do not remove the need to pass the appropriate switches to the
compiler in order to enable the relevant optimizations, that is to say
*-funroll-loops* for unrolling and *-ftree-vectorize* for
vectorization.

Pragma Loop_Variant
===================

Syntax:


::

  pragma Loop_Variant ( LOOP_VARIANT_ITEM {, LOOP_VARIANT_ITEM } );
  LOOP_VARIANT_ITEM ::= CHANGE_DIRECTION => discrete_EXPRESSION
  CHANGE_DIRECTION ::= Increases | Decreases


``Loop_Variant`` can only appear as one of the items in the sequence
of statements of a loop body, or nested inside block statements that
appear in the sequence of statements of a loop body.
It allows the specification of quantities which must always
decrease or increase in successive iterations of the loop. In its simplest
form, just one expression is specified, whose value must increase or decrease
on each iteration of the loop.

In a more complex form, multiple arguments can be given which are intepreted
in a nesting lexicographic manner. For example:

.. code-block:: ada

  pragma Loop_Variant (Increases => X, Decreases => Y);


specifies that each time through the loop either X increases, or X stays
the same and Y decreases. A ``Loop_Variant`` pragma ensures that the
loop is making progress. It can be useful in helping to show informally
or prove formally that the loop always terminates.

``Loop_Variant`` is an assertion whose effect can be controlled using
an ``Assertion_Policy`` with a check name of ``Loop_Variant``. The
policy can be ``Check`` to enable the loop variant check, ``Ignore``
to ignore the check (in which case the pragma has no effect on the program),
or ``Disable`` in which case the pragma is not even checked for correct
syntax.

Multiple ``Loop_Invariant`` and ``Loop_Variant`` pragmas that
apply to the same loop should be grouped in the same sequence of
statements.

The ``Loop_Entry`` attribute may be used within the expressions of the
``Loop_Variant`` pragma to refer to values on entry to the loop.

Pragma Machine_Attribute
========================

Syntax:


::

  pragma Machine_Attribute (
       [Entity         =>] LOCAL_NAME,
       [Attribute_Name =>] static_string_EXPRESSION
    [, [Info           =>] static_EXPRESSION {, static_EXPRESSION}] );


Machine-dependent attributes can be specified for types and/or
declarations.  This pragma is semantically equivalent to
:samp:`__attribute__(({attribute_name}))` (if ``info`` is not
specified) or :samp:`__attribute__(({attribute_name(info})))`
or :samp:`__attribute__(({attribute_name(info,...})))` in GNU C,
where *attribute_name* is recognized by the compiler middle-end
or the ``TARGET_ATTRIBUTE_TABLE`` machine specific macro.  Note
that a string literal for the optional parameter ``info`` or the
following ones is transformed by default into an identifier,
which may make this pragma unusable for some attributes.
For further information see :title:`GNU Compiler Collection (GCC) Internals`.

Pragma Main
===========

Syntax::

  pragma Main
   (MAIN_OPTION [, MAIN_OPTION]);

  MAIN_OPTION ::=
    [Stack_Size              =>] static_integer_EXPRESSION
  | [Task_Stack_Size_Default =>] static_integer_EXPRESSION
  | [Time_Slicing_Enabled    =>] static_boolean_EXPRESSION


This pragma is provided for compatibility with OpenVMS VAX Systems.  It has
no effect in GNAT, other than being syntax checked.

Pragma Main_Storage
===================

Syntax::

  pragma Main_Storage
    (MAIN_STORAGE_OPTION [, MAIN_STORAGE_OPTION]);

  MAIN_STORAGE_OPTION ::=
    [WORKING_STORAGE =>] static_SIMPLE_EXPRESSION
  | [TOP_GUARD       =>] static_SIMPLE_EXPRESSION


This pragma is provided for compatibility with OpenVMS VAX Systems.  It has
no effect in GNAT, other than being syntax checked.

.. _Pragma-Max_Queue_Length:

Pragma Max_Queue_Length
=======================

Syntax::

   pragma Max_Entry_Queue (static_integer_EXPRESSION);


This pragma is used to specify the maximum callers per entry queue for
individual protected entries and entry families. It accepts a single
integer (-1 or more) as a parameter and must appear after the declaration of an
entry.

A value of -1 represents no additional restriction on queue length.

Pragma No_Body
==============

Syntax:


.. code-block:: ada

  pragma No_Body;


There are a number of cases in which a package spec does not require a body,
and in fact a body is not permitted. GNAT will not permit the spec to be
compiled if there is a body around. The pragma No_Body allows you to provide
a body file, even in a case where no body is allowed. The body file must
contain only comments and a single No_Body pragma. This is recognized by
the compiler as indicating that no body is logically present.

This is particularly useful during maintenance when a package is modified in
such a way that a body needed before is no longer needed. The provision of a
dummy body with a No_Body pragma ensures that there is no interference from
earlier versions of the package body.

.. _Pragma-No_Caching:

Pragma No_Caching
=================

Syntax:

.. code-block:: ada

  pragma No_Caching [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect ``No_Caching`` in
the SPARK 2014 Reference Manual, section 7.1.2.

Pragma No_Component_Reordering
==============================

Syntax:


::

  pragma No_Component_Reordering [([Entity =>] type_LOCAL_NAME)];


``type_LOCAL_NAME`` must refer to a record type declaration in the current
declarative part. The effect is to preclude any reordering of components
for the layout of the record, i.e. the record is laid out by the compiler
in the order in which the components are declared textually. The form with
no argument is a configuration pragma which applies to all record types
declared in units to which the pragma applies and there is a requirement
that this pragma be used consistently within a partition.

.. _Pragma-No_Elaboration_Code_All:

Pragma No_Elaboration_Code_All
==============================

Syntax:


::

  pragma No_Elaboration_Code_All [(program_unit_NAME)];


This is a program unit pragma (there is also an equivalent aspect of the
same name) that establishes the restriction ``No_Elaboration_Code`` for
the current unit and any extended main source units (body and subunits).
It also has the effect of enforcing a transitive application of this
aspect, so that if any unit is implicitly or explicitly with'ed by the
current unit, it must also have the No_Elaboration_Code_All aspect set.
It may be applied to package or subprogram specs or their generic versions.

Pragma No_Heap_Finalization
===========================

Syntax:


::

  pragma No_Heap_Finalization [ (first_subtype_LOCAL_NAME) ];


Pragma ``No_Heap_Finalization`` may be used as a configuration pragma or as a
type-specific pragma.

In its configuration form, the pragma must appear within a configuration file
such as gnat.adc, without an argument. The pragma suppresses the call to
``Finalize`` for heap-allocated objects created through library-level named
access-to-object types in cases where the designated type requires finalization
actions.

In its type-specific form, the argument of the pragma must denote a
library-level named access-to-object type. The pragma suppresses the call to
``Finalize`` for heap-allocated objects created through the specific access type
in cases where the designated type requires finalization actions.

It is still possible to finalize such heap-allocated objects by explicitly
deallocating them.

A library-level named access-to-object type declared within a generic unit will
lose its ``No_Heap_Finalization`` pragma when the corresponding instance does not
appear at the library level.

.. _Pragma-No_Inline:

Pragma No_Inline
================

Syntax:


::

  pragma No_Inline (NAME {, NAME});


This pragma suppresses inlining for the callable entity or the instances of
the generic subprogram designated by ``NAME``, including inlining that
results from the use of pragma ``Inline``.  This pragma is always active,
in particular it is not subject to the use of option *-gnatn* or
*-gnatN*.  It is illegal to specify both pragma ``No_Inline`` and
pragma ``Inline_Always`` for the same ``NAME``.

Pragma No_Return
================

Syntax:


::

  pragma No_Return (procedure_LOCAL_NAME {, procedure_LOCAL_NAME});


Each ``procedure_LOCAL_NAME`` argument must refer to one or more procedure
declarations in the current declarative part.  A procedure to which this
pragma is applied may not contain any explicit ``return`` statements.
In addition, if the procedure contains any implicit returns from falling
off the end of a statement sequence, then execution of that implicit
return will cause Program_Error to be raised.

One use of this pragma is to identify procedures whose only purpose is to raise
an exception. Another use of this pragma is to suppress incorrect warnings
about missing returns in functions, where the last statement of a function
statement sequence is a call to such a procedure.

Note that in Ada 2005 mode, this pragma is part of the language. It is
available in all earlier versions of Ada as an implementation-defined
pragma.

Pragma No_Strict_Aliasing
=========================

Syntax:


::

  pragma No_Strict_Aliasing [([Entity =>] type_LOCAL_NAME)];


``type_LOCAL_NAME`` must refer to an access type
declaration in the current declarative part.  The effect is to inhibit
strict aliasing optimization for the given type.  The form with no
arguments is a configuration pragma which applies to all access types
declared in units to which the pragma applies. For a detailed
description of the strict aliasing optimization, and the situations
in which it must be suppressed, see the section on Optimization and Strict Aliasing
in the :title:`GNAT User's Guide`.

This pragma currently has no effects on access to unconstrained array types.

.. _Pragma-No_Tagged_Streams:

Pragma No_Tagged_Streams
========================

Syntax:


::

  pragma No_Tagged_Streams [([Entity =>] tagged_type_LOCAL_NAME)];


Normally when a tagged type is introduced using a full type declaration,
part of the processing includes generating stream access routines to be
used by stream attributes referencing the type (or one of its subtypes
or derived types). This can involve the generation of significant amounts
of code which is wasted space if stream routines are not needed for the
type in question.

The ``No_Tagged_Streams`` pragma causes the generation of these stream
routines to be skipped, and any attempt to use stream operations on
types subject to this pragma will be statically rejected as illegal.

There are two forms of the pragma. The form with no arguments must appear
in a declarative sequence or in the declarations of a package spec. This
pragma affects all subsequent root tagged types declared in the declaration
sequence, and specifies that no stream routines be generated. The form with
an argument (for which there is also a corresponding aspect) specifies a
single root tagged type for which stream routines are not to be generated.

Once the pragma has been given for a particular root tagged type, all subtypes
and derived types of this type inherit the pragma automatically, so the effect
applies to a complete hierarchy (this is necessary to deal with the class-wide
dispatching versions of the stream routines).

When pragmas ``Discard_Names`` and ``No_Tagged_Streams`` are simultaneously
applied to a tagged type its Expanded_Name and External_Tag are initialized
with empty strings. This is useful to avoid exposing entity names at binary
level but has a negative impact on the debuggability of tagged types.

Pragma Normalize_Scalars
========================

Syntax:


.. code-block:: ada

  pragma Normalize_Scalars;


This is a language defined pragma which is fully implemented in GNAT.  The
effect is to cause all scalar objects that are not otherwise initialized
to be initialized.  The initial values are implementation dependent and
are as follows:



*Standard.Character*
  Objects whose root type is Standard.Character are initialized to
  Character'Last unless the subtype range excludes NUL (in which case
  NUL is used). This choice will always generate an invalid value if
  one exists.


*Standard.Wide_Character*
  Objects whose root type is Standard.Wide_Character are initialized to
  Wide_Character'Last unless the subtype range excludes NUL (in which case
  NUL is used). This choice will always generate an invalid value if
  one exists.


*Standard.Wide_Wide_Character*
  Objects whose root type is Standard.Wide_Wide_Character are initialized to
  the invalid value 16#FFFF_FFFF# unless the subtype range excludes NUL (in
  which case NUL is used). This choice will always generate an invalid value if
  one exists.


*Integer types*
  Objects of an integer type are treated differently depending on whether
  negative values are present in the subtype. If no negative values are
  present, then all one bits is used as the initial value except in the
  special case where zero is excluded from the subtype, in which case
  all zero bits are used. This choice will always generate an invalid
  value if one exists.

  For subtypes with negative values present, the largest negative number
  is used, except in the unusual case where this largest negative number
  is in the subtype, and the largest positive number is not, in which case
  the largest positive value is used. This choice will always generate
  an invalid value if one exists.


*Floating-Point Types*
  Objects of all floating-point types are initialized to all 1-bits. For
  standard IEEE format, this corresponds to a NaN (not a number) which is
  indeed an invalid value.


*Fixed-Point Types*
  Objects of all fixed-point types are treated as described above for integers,
  with the rules applying to the underlying integer value used to represent
  the fixed-point value.


*Modular types*
  Objects of a modular type are initialized to all one bits, except in
  the special case where zero is excluded from the subtype, in which
  case all zero bits are used. This choice will always generate an
  invalid value if one exists.


*Enumeration types*
  Objects of an enumeration type are initialized to all one-bits, i.e., to
  the value ``2 ** typ'Size - 1`` unless the subtype excludes the literal
  whose Pos value is zero, in which case a code of zero is used. This choice
  will always generate an invalid value if one exists.

.. _Pragma_Obsolescent:

Pragma Obsolescent
==================

Syntax:


::

  pragma Obsolescent;

  pragma Obsolescent (
    [Message =>] static_string_EXPRESSION
  [,[Version =>] Ada_05]]);

  pragma Obsolescent (
    [Entity  =>] NAME
  [,[Message =>] static_string_EXPRESSION
  [,[Version =>] Ada_05]] );


This pragma can occur immediately following a declaration of an entity,
including the case of a record component. If no Entity argument is present,
then this declaration is the one to which the pragma applies. If an Entity
parameter is present, it must either match the name of the entity in this
declaration, or alternatively, the pragma can immediately follow an enumeration
type declaration, where the Entity argument names one of the enumeration
literals.

This pragma is used to indicate that the named entity
is considered obsolescent and should not be used. Typically this is
used when an API must be modified by eventually removing or modifying
existing subprograms or other entities. The pragma can be used at an
intermediate stage when the entity is still present, but will be
removed later.

The effect of this pragma is to output a warning message on a reference to
an entity thus marked that the subprogram is obsolescent if the appropriate
warning option in the compiler is activated. If the ``Message`` parameter is
present, then a second warning message is given containing this text. In
addition, a reference to the entity is considered to be a violation of pragma
``Restrictions (No_Obsolescent_Features)``.

This pragma can also be used as a program unit pragma for a package,
in which case the entity name is the name of the package, and the
pragma indicates that the entire package is considered
obsolescent. In this case a client ``with``\ ing such a package
violates the restriction, and the ``with`` clause is
flagged with warnings if the warning option is set.

If the ``Version`` parameter is present (which must be exactly
the identifier ``Ada_05``, no other argument is allowed), then the
indication of obsolescence applies only when compiling in Ada 2005
mode. This is primarily intended for dealing with the situations
in the predefined library where subprograms or packages
have become defined as obsolescent in Ada 2005
(e.g., in ``Ada.Characters.Handling``), but may be used anywhere.

The following examples show typical uses of this pragma:


.. code-block:: ada

  package p is
     pragma Obsolescent (p, Message => "use pp instead of p");
  end p;

  package q is
     procedure q2;
     pragma Obsolescent ("use q2new instead");

     type R is new integer;
     pragma Obsolescent
       (Entity  => R,
        Message => "use RR in Ada 2005",
        Version => Ada_05);

     type M is record
        F1 : Integer;
        F2 : Integer;
        pragma Obsolescent;
        F3 : Integer;
     end record;

     type E is (a, bc, 'd', quack);
     pragma Obsolescent (Entity => bc)
     pragma Obsolescent (Entity => 'd')

     function "+"
       (a, b : character) return character;
     pragma Obsolescent (Entity => "+");
  end;


Note that, as for all pragmas, if you use a pragma argument identifier,
then all subsequent parameters must also use a pragma argument identifier.
So if you specify ``Entity =>`` for the ``Entity`` argument, and a ``Message``
argument is present, it must be preceded by ``Message =>``.

Pragma Optimize_Alignment
=========================
.. index:: Alignment, default settings

Syntax:


.. code-block:: ada

  pragma Optimize_Alignment (TIME | SPACE | OFF);


This is a configuration pragma which affects the choice of default alignments
for types and objects where no alignment is explicitly specified. There is a
time/space trade-off in the selection of these values. Large alignments result
in more efficient code, at the expense of larger data space, since sizes have
to be increased to match these alignments. Smaller alignments save space, but
the access code is slower. The normal choice of default alignments for types
and individual alignment promotions for objects (which is what you get if you
do not use this pragma, or if you use an argument of OFF), tries to balance
these two requirements.

Specifying SPACE causes smaller default alignments to be chosen in two cases.
First any packed record is given an alignment of 1. Second, if a size is given
for the type, then the alignment is chosen to avoid increasing this size. For
example, consider:


.. code-block:: ada

     type R is record
        X : Integer;
        Y : Character;
     end record;

     for R'Size use 5*8;


In the default mode, this type gets an alignment of 4, so that access to the
Integer field X are efficient. But this means that objects of the type end up
with a size of 8 bytes. This is a valid choice, since sizes of objects are
allowed to be bigger than the size of the type, but it can waste space if for
example fields of type R appear in an enclosing record. If the above type is
compiled in ``Optimize_Alignment (Space)`` mode, the alignment is set to 1.

However, there is one case in which SPACE is ignored. If a variable length
record (that is a discriminated record with a component which is an array
whose length depends on a discriminant), has a pragma Pack, then it is not
in general possible to set the alignment of such a record to one, so the
pragma is ignored in this case (with a warning).

Specifying SPACE also disables alignment promotions for standalone objects,
which occur when the compiler increases the alignment of a specific object
without changing the alignment of its type.

Specifying SPACE also disables component reordering in unpacked record types,
which can result in larger sizes in order to meet alignment requirements.

Specifying TIME causes larger default alignments to be chosen in the case of
small types with sizes that are not a power of 2. For example, consider:


.. code-block:: ada

     type R is record
        A : Character;
        B : Character;
        C : Boolean;
     end record;

     pragma Pack (R);
     for R'Size use 17;


The default alignment for this record is normally 1, but if this type is
compiled in ``Optimize_Alignment (Time)`` mode, then the alignment is set
to 4, which wastes space for objects of the type, since they are now 4 bytes
long, but results in more efficient access when the whole record is referenced.

As noted above, this is a configuration pragma, and there is a requirement
that all units in a partition be compiled with a consistent setting of the
optimization setting. This would normally be achieved by use of a configuration
pragma file containing the appropriate setting. The exception to this rule is
that units with an explicit configuration pragma in the same file as the source
unit are excluded from the consistency check, as are all predefined units. The
latter are compiled by default in pragma Optimize_Alignment (Off) mode if no
pragma appears at the start of the file.

Pragma Ordered
==============

Syntax:


.. code-block:: ada

  pragma Ordered (enumeration_first_subtype_LOCAL_NAME);


Most enumeration types are from a conceptual point of view unordered.
For example, consider:


.. code-block:: ada

  type Color is (Red, Blue, Green, Yellow);


By Ada semantics ``Blue > Red`` and ``Green > Blue``,
but really these relations make no sense; the enumeration type merely
specifies a set of possible colors, and the order is unimportant.

For unordered enumeration types, it is generally a good idea if
clients avoid comparisons (other than equality or inequality) and
explicit ranges. (A *client* is a unit where the type is referenced,
other than the unit where the type is declared, its body, and its subunits.)
For example, if code buried in some client says:


.. code-block:: ada

  if Current_Color < Yellow then ...
  if Current_Color in Blue .. Green then ...


then the client code is relying on the order, which is undesirable.
It makes the code hard to read and creates maintenance difficulties if
entries have to be added to the enumeration type. Instead,
the code in the client should list the possibilities, or an
appropriate subtype should be declared in the unit that declares
the original enumeration type. E.g., the following subtype could
be declared along with the type ``Color``:


.. code-block:: ada

  subtype RBG is Color range Red .. Green;


and then the client could write:


.. code-block:: ada

  if Current_Color in RBG then ...
  if Current_Color = Blue or Current_Color = Green then ...


However, some enumeration types are legitimately ordered from a conceptual
point of view. For example, if you declare:


.. code-block:: ada

  type Day is (Mon, Tue, Wed, Thu, Fri, Sat, Sun);


then the ordering imposed by the language is reasonable, and
clients can depend on it, writing for example:


.. code-block:: ada

  if D in Mon .. Fri then ...
  if D < Wed then ...


The pragma *Ordered* is provided to mark enumeration types that
are conceptually ordered, alerting the reader that clients may depend
on the ordering. GNAT provides a pragma to mark enumerations as ordered
rather than one to mark them as unordered, since in our experience,
the great majority of enumeration types are conceptually unordered.

The types ``Boolean``, ``Character``, ``Wide_Character``,
and ``Wide_Wide_Character``
are considered to be ordered types, so each is declared with a
pragma ``Ordered`` in package ``Standard``.

Normally pragma ``Ordered`` serves only as documentation and a guide for
coding standards, but GNAT provides a warning switch *-gnatw.u* that
requests warnings for inappropriate uses (comparisons and explicit
subranges) for unordered types. If this switch is used, then any
enumeration type not marked with pragma ``Ordered`` will be considered
as unordered, and will generate warnings for inappropriate uses.

Note that generic types are not considered ordered or unordered (since the
template can be instantiated for both cases), so we never generate warnings
for the case of generic enumerated types.

For additional information please refer to the description of the
*-gnatw.u* switch in the GNAT User's Guide.

Pragma Overflow_Mode
====================

Syntax:


::

  pragma Overflow_Mode
   (  [General    =>] MODE
    [,[Assertions =>] MODE]);

  MODE ::= STRICT | MINIMIZED | ELIMINATED


This pragma sets the current overflow mode to the given setting. For details
of the meaning of these modes, please refer to the
'Overflow Check Handling in GNAT' appendix in the
GNAT User's Guide. If only the ``General`` parameter is present,
the given mode applies to all expressions. If both parameters are present,
the ``General`` mode applies to expressions outside assertions, and
the ``Eliminated`` mode applies to expressions within assertions.

The case of the ``MODE`` parameter is ignored,
so ``MINIMIZED``, ``Minimized`` and
``minimized`` all have the same effect.

The ``Overflow_Mode`` pragma has the same scoping and placement
rules as pragma ``Suppress``, so it can occur either as a
configuration pragma, specifying a default for the whole
program, or in a declarative scope, where it applies to the
remaining declarations and statements in that scope.

The pragma ``Suppress (Overflow_Check)`` suppresses
overflow checking, but does not affect the overflow mode.

The pragma ``Unsuppress (Overflow_Check)`` unsuppresses (enables)
overflow checking, but does not affect the overflow mode.

Pragma Overriding_Renamings
===========================
.. index:: Rational profile

.. index:: Rational compatibility

Syntax:


.. code-block:: ada

  pragma Overriding_Renamings;


This is a GNAT configuration pragma to simplify porting
legacy code accepted by the Rational
Ada compiler. In the presence of this pragma, a renaming declaration that
renames an inherited operation declared in the same scope is legal if selected
notation is used as in:


.. code-block:: ada

  pragma Overriding_Renamings;
  ...
  package R is
    function F (..);
    ...
    function F (..) renames R.F;
  end R;


even though
RM 8.3 (15) stipulates that an overridden operation is not visible within the
declaration of the overriding operation.

Pragma Partition_Elaboration_Policy
===================================

Syntax:


::

  pragma Partition_Elaboration_Policy (POLICY_IDENTIFIER);

  POLICY_IDENTIFIER ::= Concurrent | Sequential


This pragma is standard in Ada 2005, but is available in all earlier
versions of Ada as an implementation-defined pragma.
See Ada 2012 Reference Manual for details.

.. _Pragma-Part_Of:

Pragma Part_Of
==============

Syntax:

.. code-block:: ada

  pragma Part_Of (ABSTRACT_STATE);

  ABSTRACT_STATE ::= NAME

For the semantics of this pragma, see the entry for aspect ``Part_Of`` in the
SPARK 2014 Reference Manual, section 7.2.6.

Pragma Passive
==============

Syntax:


::

  pragma Passive [(Semaphore | No)];


Syntax checked, but otherwise ignored by GNAT.  This is recognized for
compatibility with DEC Ada 83 implementations, where it is used within a
task definition to request that a task be made passive.  If the argument
``Semaphore`` is present, or the argument is omitted, then DEC Ada 83
treats the pragma as an assertion that the containing task is passive
and that optimization of context switch with this task is permitted and
desired.  If the argument ``No`` is present, the task must not be
optimized.  GNAT does not attempt to optimize any tasks in this manner
(since protected objects are available in place of passive tasks).

For more information on the subject of passive tasks, see the section
'Passive Task Optimization' in the GNAT Users Guide.

.. _Pragma-Persistent_BSS:

Pragma Persistent_BSS
=====================

Syntax:


::

  pragma Persistent_BSS [(LOCAL_NAME)]


This pragma allows selected objects to be placed in the ``.persistent_bss``
section. On some targets the linker and loader provide for special
treatment of this section, allowing a program to be reloaded without
affecting the contents of this data (hence the name persistent).

There are two forms of usage. If an argument is given, it must be the
local name of a library-level object, with no explicit initialization
and whose type is potentially persistent. If no argument is given, then
the pragma is a configuration pragma, and applies to all library-level
objects with no explicit initialization of potentially persistent types.

A potentially persistent type is a scalar type, or an untagged,
non-discriminated record, all of whose components have no explicit
initialization and are themselves of a potentially persistent type,
or an array, all of whose constraints are static, and whose component
type is potentially persistent.

If this pragma is used on a target where this feature is not supported,
then the pragma will be ignored. See also ``pragma Linker_Section``.

Pragma Post
===========
.. index:: Post

.. index:: Checks, postconditions


Syntax:


.. code-block:: ada

  pragma Post (Boolean_Expression);


The ``Post`` pragma is intended to be an exact replacement for
the language-defined
``Post`` aspect, and shares its restrictions and semantics.
It must appear either immediately following the corresponding
subprogram declaration (only other pragmas may intervene), or
if there is no separate subprogram declaration, then it can
appear at the start of the declarations in a subprogram body
(preceded only by other pragmas).

Pragma Postcondition
====================
.. index:: Postcondition

.. index:: Checks, postconditions


Syntax:


::

  pragma Postcondition (
     [Check   =>] Boolean_Expression
   [,[Message =>] String_Expression]);


The ``Postcondition`` pragma allows specification of automatic
postcondition checks for subprograms. These checks are similar to
assertions, but are automatically inserted just prior to the return
statements of the subprogram with which they are associated (including
implicit returns at the end of procedure bodies and associated
exception handlers).

In addition, the boolean expression which is the condition which
must be true may contain references to function'Result in the case
of a function to refer to the returned value.

``Postcondition`` pragmas may appear either immediately following the
(separate) declaration of a subprogram, or at the start of the
declarations of a subprogram body. Only other pragmas may intervene
(that is appear between the subprogram declaration and its
postconditions, or appear before the postcondition in the
declaration sequence in a subprogram body). In the case of a
postcondition appearing after a subprogram declaration, the
formal arguments of the subprogram are visible, and can be
referenced in the postcondition expressions.

The postconditions are collected and automatically tested just
before any return (implicit or explicit) in the subprogram body.
A postcondition is only recognized if postconditions are active
at the time the pragma is encountered. The compiler switch *gnata*
turns on all postconditions by default, and pragma ``Check_Policy``
with an identifier of ``Postcondition`` can also be used to
control whether postconditions are active.

The general approach is that postconditions are placed in the spec
if they represent functional aspects which make sense to the client.
For example we might have:


.. code-block:: ada

     function Direction return Integer;
     pragma Postcondition
      (Direction'Result = +1
         or else
       Direction'Result = -1);


which serves to document that the result must be +1 or -1, and
will test that this is the case at run time if postcondition
checking is active.

Postconditions within the subprogram body can be used to
check that some internal aspect of the implementation,
not visible to the client, is operating as expected.
For instance if a square root routine keeps an internal
counter of the number of times it is called, then we
might have the following postcondition:


.. code-block:: ada

     Sqrt_Calls : Natural := 0;

     function Sqrt (Arg : Float) return Float is
       pragma Postcondition
         (Sqrt_Calls = Sqrt_Calls'Old + 1);
       ...
     end Sqrt


As this example, shows, the use of the ``Old`` attribute
is often useful in postconditions to refer to the state on
entry to the subprogram.

Note that postconditions are only checked on normal returns
from the subprogram. If an abnormal return results from
raising an exception, then the postconditions are not checked.

If a postcondition fails, then the exception
``System.Assertions.Assert_Failure`` is raised. If
a message argument was supplied, then the given string
will be used as the exception message. If no message
argument was supplied, then the default message has
the form "Postcondition failed at file_name:line". The
exception is raised in the context of the subprogram
body, so it is possible to catch postcondition failures
within the subprogram body itself.

Within a package spec, normal visibility rules
in Ada would prevent forward references within a
postcondition pragma to functions defined later in
the same package. This would introduce undesirable
ordering constraints. To avoid this problem, all
postcondition pragmas are analyzed at the end of
the package spec, allowing forward references.

The following example shows that this even allows
mutually recursive postconditions as in:


.. code-block:: ada

  package Parity_Functions is
     function Odd  (X : Natural) return Boolean;
     pragma Postcondition
       (Odd'Result =
          (x = 1
            or else
          (x /= 0 and then Even (X - 1))));

     function Even (X : Natural) return Boolean;
     pragma Postcondition
       (Even'Result =
          (x = 0
            or else
          (x /= 1 and then Odd (X - 1))));

  end Parity_Functions;


There are no restrictions on the complexity or form of
conditions used within ``Postcondition`` pragmas.
The following example shows that it is even possible
to verify performance behavior.


.. code-block:: ada

  package Sort is

     Performance : constant Float;
     --  Performance constant set by implementation
     --  to match target architecture behavior.

     procedure Treesort (Arg : String);
     --  Sorts characters of argument using N*logN sort
     pragma Postcondition
       (Float (Clock - Clock'Old) <=
          Float (Arg'Length) *
          log (Float (Arg'Length)) *
          Performance);
  end Sort;


Note: postcondition pragmas associated with subprograms that are
marked as Inline_Always, or those marked as Inline with front-end
inlining (-gnatN option set) are accepted and legality-checked
by the compiler, but are ignored at run-time even if postcondition
checking is enabled.

Note that pragma ``Postcondition`` differs from the language-defined
``Post`` aspect (and corresponding ``Post`` pragma) in allowing
multiple occurrences, allowing occurences in the body even if there
is a separate spec, and allowing a second string parameter, and the
use of the pragma identifier ``Check``. Historically, pragma
``Postcondition`` was implemented prior to the development of
Ada 2012, and has been retained in its original form for
compatibility purposes.

Pragma Post_Class
=================
.. index:: Post

.. index:: Checks, postconditions


Syntax:


.. code-block:: ada

  pragma Post_Class (Boolean_Expression);


The ``Post_Class`` pragma is intended to be an exact replacement for
the language-defined
``Post'Class`` aspect, and shares its restrictions and semantics.
It must appear either immediately following the corresponding
subprogram declaration (only other pragmas may intervene), or
if there is no separate subprogram declaration, then it can
appear at the start of the declarations in a subprogram body
(preceded only by other pragmas).

Note: This pragma is called ``Post_Class`` rather than
``Post'Class`` because the latter would not be strictly
conforming to the allowed syntax for pragmas. The motivation
for provinding pragmas equivalent to the aspects is to allow a program
to be written using the pragmas, and then compiled if necessary
using an Ada compiler that does not recognize the pragmas or
aspects, but is prepared to ignore the pragmas. The assertion
policy that controls this pragma is ``Post'Class``, not
``Post_Class``.

Pragma Pre
==========
.. index:: Pre

.. index:: Checks, preconditions


Syntax:


.. code-block:: ada

  pragma Pre (Boolean_Expression);


The ``Pre`` pragma is intended to be an exact replacement for
the language-defined
``Pre`` aspect, and shares its restrictions and semantics.
It must appear either immediately following the corresponding
subprogram declaration (only other pragmas may intervene), or
if there is no separate subprogram declaration, then it can
appear at the start of the declarations in a subprogram body
(preceded only by other pragmas).

Pragma Precondition
===================
.. index:: Preconditions

.. index:: Checks, preconditions


Syntax:


::

  pragma Precondition (
     [Check   =>] Boolean_Expression
   [,[Message =>] String_Expression]);


The ``Precondition`` pragma is similar to ``Postcondition``
except that the corresponding checks take place immediately upon
entry to the subprogram, and if a precondition fails, the exception
is raised in the context of the caller, and the attribute 'Result
cannot be used within the precondition expression.

Otherwise, the placement and visibility rules are identical to those
described for postconditions. The following is an example of use
within a package spec:


.. code-block:: ada

  package Math_Functions is
     ...
     function Sqrt (Arg : Float) return Float;
     pragma Precondition (Arg >= 0.0)
     ...
  end Math_Functions;


``Precondition`` pragmas may appear either immediately following the
(separate) declaration of a subprogram, or at the start of the
declarations of a subprogram body. Only other pragmas may intervene
(that is appear between the subprogram declaration and its
postconditions, or appear before the postcondition in the
declaration sequence in a subprogram body).

Note: precondition pragmas associated with subprograms that are
marked as Inline_Always, or those marked as Inline with front-end
inlining (-gnatN option set) are accepted and legality-checked
by the compiler, but are ignored at run-time even if precondition
checking is enabled.

Note that pragma ``Precondition`` differs from the language-defined
``Pre`` aspect (and corresponding ``Pre`` pragma) in allowing
multiple occurrences, allowing occurences in the body even if there
is a separate spec, and allowing a second string parameter, and the
use of the pragma identifier ``Check``. Historically, pragma
``Precondition`` was implemented prior to the development of
Ada 2012, and has been retained in its original form for
compatibility purposes.

.. _Pragma-Predicate:

Pragma Predicate
================

Syntax:


::

  pragma Predicate
    ([Entity =>] type_LOCAL_NAME,
     [Check  =>] EXPRESSION);


This pragma (available in all versions of Ada in GNAT) encompasses both
the ``Static_Predicate`` and ``Dynamic_Predicate`` aspects in
Ada 2012. A predicate is regarded as static if it has an allowed form
for ``Static_Predicate`` and is otherwise treated as a
``Dynamic_Predicate``. Otherwise, predicates specified by this
pragma behave exactly as described in the Ada 2012 reference manual.
For example, if we have


.. code-block:: ada

  type R is range 1 .. 10;
  subtype S is R;
  pragma Predicate (Entity => S, Check => S not in 4 .. 6);
  subtype Q is R
  pragma Predicate (Entity => Q, Check => F(Q) or G(Q));


the effect is identical to the following Ada 2012 code:


.. code-block:: ada

  type R is range 1 .. 10;
  subtype S is R with
    Static_Predicate => S not in 4 .. 6;
  subtype Q is R with
    Dynamic_Predicate => F(Q) or G(Q);


Note that there are no pragmas ``Dynamic_Predicate``
or ``Static_Predicate``. That is
because these pragmas would affect legality and semantics of
the program and thus do not have a neutral effect if ignored.
The motivation behind providing pragmas equivalent to
corresponding aspects is to allow a program to be written
using the pragmas, and then compiled with a compiler that
will ignore the pragmas. That doesn't work in the case of
static and dynamic predicates, since if the corresponding
pragmas are ignored, then the behavior of the program is
fundamentally changed (for example a membership test
``A in B`` would not take into account a predicate
defined for subtype B). When following this approach, the
use of predicates should be avoided.

Pragma Predicate_Failure
========================

Syntax:


::

  pragma Predicate_Failure
    ([Entity  =>] type_LOCAL_NAME,
     [Message =>] String_Expression);


The ``Predicate_Failure`` pragma is intended to be an exact replacement for
the language-defined
``Predicate_Failure`` aspect, and shares its restrictions and semantics.

Pragma Preelaborable_Initialization
===================================

Syntax:


.. code-block:: ada

  pragma Preelaborable_Initialization (DIRECT_NAME);


This pragma is standard in Ada 2005, but is available in all earlier
versions of Ada as an implementation-defined pragma.
See Ada 2012 Reference Manual for details.

Pragma Prefix_Exception_Messages
================================
.. index:: Prefix_Exception_Messages

.. index:: exception

.. index:: Exception_Message


Syntax:


.. code-block:: ada

  pragma Prefix_Exception_Messages;


This is an implementation-defined configuration pragma that affects the
behavior of raise statements with a message given as a static string
constant (typically a string literal). In such cases, the string will
be automatically prefixed by the name of the enclosing entity (giving
the package and subprogram containing the raise statement). This helps
to identify where messages are coming from, and this mode is automatic
for the run-time library.

The pragma has no effect if the message is computed with an expression other
than a static string constant, since the assumption in this case is that
the program computes exactly the string it wants. If you still want the
prefixing in this case, you can always call
``GNAT.Source_Info.Enclosing_Entity`` and prepend the string manually.

Pragma Pre_Class
================
.. index:: Pre_Class

.. index:: Checks, preconditions


Syntax:


.. code-block:: ada

  pragma Pre_Class (Boolean_Expression);


The ``Pre_Class`` pragma is intended to be an exact replacement for
the language-defined
``Pre'Class`` aspect, and shares its restrictions and semantics.
It must appear either immediately following the corresponding
subprogram declaration (only other pragmas may intervene), or
if there is no separate subprogram declaration, then it can
appear at the start of the declarations in a subprogram body
(preceded only by other pragmas).

Note: This pragma is called ``Pre_Class`` rather than
``Pre'Class`` because the latter would not be strictly
conforming to the allowed syntax for pragmas. The motivation
for providing pragmas equivalent to the aspects is to allow a program
to be written using the pragmas, and then compiled if necessary
using an Ada compiler that does not recognize the pragmas or
aspects, but is prepared to ignore the pragmas. The assertion
policy that controls this pragma is ``Pre'Class``, not
``Pre_Class``.

Pragma Priority_Specific_Dispatching
====================================

Syntax:


::

  pragma Priority_Specific_Dispatching (
     POLICY_IDENTIFIER,
     first_priority_EXPRESSION,
     last_priority_EXPRESSION)

  POLICY_IDENTIFIER ::=
     EDF_Across_Priorities            |
     FIFO_Within_Priorities           |
     Non_Preemptive_Within_Priorities |
     Round_Robin_Within_Priorities


This pragma is standard in Ada 2005, but is available in all earlier
versions of Ada as an implementation-defined pragma.
See Ada 2012 Reference Manual for details.

Pragma Profile
==============

Syntax:


.. code-block:: ada

  pragma Profile (Ravenscar | Restricted | Rational | Jorvik |
                  GNAT_Extended_Ravenscar | GNAT_Ravenscar_EDF );


This pragma is standard in Ada 2005, but is available in all earlier
versions of Ada as an implementation-defined pragma. This is a
configuration pragma that establishes a set of configuration pragmas
that depend on the argument. ``Ravenscar`` is standard in Ada 2005.
``Jorvik`` is standard in Ada 202x.
The other possibilities (``Restricted``, ``Rational``,
``GNAT_Extended_Ravenscar``, ``GNAT_Ravenscar_EDF``)
are implementation-defined.  ``GNAT_Extended_Ravenscar`` is an alias for ``Jorvik``.

The set of configuration pragmas is defined in the following sections.


* Pragma Profile (Ravenscar)

  The ``Ravenscar`` profile is standard in Ada 2005,
  but is available in all earlier
  versions of Ada as an implementation-defined pragma. This profile
  establishes the following set of configuration pragmas:

  * ``Task_Dispatching_Policy (FIFO_Within_Priorities)``

    [RM D.2.2] Tasks are dispatched following a preemptive
    priority-ordered scheduling policy.


  * ``Locking_Policy (Ceiling_Locking)``

    [RM D.3] While tasks and interrupts execute a protected action, they inherit
    the ceiling priority of the corresponding protected object.


  * ``Detect_Blocking``

    This pragma forces the detection of potentially blocking operations within a
    protected operation, and to raise Program_Error if that happens.

  plus the following set of restrictions:

  * ``Max_Entry_Queue_Length => 1``

    No task can be queued on a protected entry.

  * ``Max_Protected_Entries => 1``

  * ``Max_Task_Entries => 0``

    No rendezvous statements are allowed.

  * ``No_Abort_Statements``

  * ``No_Dynamic_Attachment``

  * ``No_Dynamic_Priorities``

  * ``No_Implicit_Heap_Allocations``

  * ``No_Local_Protected_Objects``

  * ``No_Local_Timing_Events``

  * ``No_Protected_Type_Allocators``

  * ``No_Relative_Delay``

  * ``No_Requeue_Statements``

  * ``No_Select_Statements``

  * ``No_Specific_Termination_Handlers``

  * ``No_Task_Allocators``

  * ``No_Task_Hierarchy``

  * ``No_Task_Termination``

  * ``Simple_Barriers``

  The Ravenscar profile also includes the following restrictions that specify
  that there are no semantic dependencies on the corresponding predefined
  packages:

  * ``No_Dependence => Ada.Asynchronous_Task_Control``

  * ``No_Dependence => Ada.Calendar``

  * ``No_Dependence => Ada.Execution_Time.Group_Budget``

  * ``No_Dependence => Ada.Execution_Time.Timers``

  * ``No_Dependence => Ada.Task_Attributes``

  * ``No_Dependence => System.Multiprocessors.Dispatching_Domains``

  This set of configuration pragmas and restrictions correspond to the
  definition of the 'Ravenscar Profile' for limited tasking, devised and
  published by the :title:`International Real-Time Ada Workshop, 1997`.
  A description is also available at
  `http://www-users.cs.york.ac.uk/~burns/ravenscar.ps <http://www-users.cs.york.ac.uk/~burns/ravenscar.ps>`_.

  The original definition of the profile was revised at subsequent IRTAW
  meetings. It has been included in the ISO
  :title:`Guide for the Use of the Ada Programming Language in High Integrity Systems`,
  and was made part of the Ada 2005 standard.
  The formal definition given by
  the Ada Rapporteur Group (ARG) can be found in two Ada Issues (AI-249 and
  AI-305) available at
  `http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00249.txt <http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00249.txt>`_ and
  `http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00305.txt <http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ais/ai-00305.txt>`_.

  The above set is a superset of the restrictions provided by pragma
  ``Profile (Restricted)``, it includes six additional restrictions
  (``Simple_Barriers``, ``No_Select_Statements``,
  ``No_Calendar``, ``No_Implicit_Heap_Allocations``,
  ``No_Relative_Delay`` and ``No_Task_Termination``).  This means
  that pragma ``Profile (Ravenscar)``, like the pragma
  ``Profile (Restricted)``,
  automatically causes the use of a simplified,
  more efficient version of the tasking run-time library.

* Pragma Profile (Jorvik)

  ``Jorvik`` is the new profile added to the Ada 202x draft standard,
  previously implemented under the name ``GNAT_Extended_Ravenscar``.

  The ``No_Implicit_Heap_Allocations`` restriction has been replaced
  by ``No_Implicit_Task_Allocations`` and
  ``No_Implicit_Protected_Object_Allocations``.

  The ``Simple_Barriers`` restriction has been replaced by
  ``Pure_Barriers``.

  The ``Max_Protected_Entries``, ``Max_Entry_Queue_Length``, and
  ``No_Relative_Delay`` restrictions have been removed.

  Details on the rationale for ``Jorvik`` and implications for use may be
  found in :title:`A New Ravenscar-Based Profile` by P. Rogers, J. Ruiz,
  T. Gingold and P. Bernardi, in :title:`Reliable Software Technologies --
  Ada Europe 2017`, Springer-Verlag Lecture Notes in Computer Science,
  Number 10300.


* Pragma Profile (GNAT_Ravenscar_EDF)

  This profile corresponds to the Ravenscar profile but using
  EDF_Across_Priority as the Task_Scheduling_Policy.

* Pragma Profile (Restricted)

  This profile corresponds to the GNAT restricted run time. It
  establishes the following set of restrictions:

  * ``No_Abort_Statements``
  * ``No_Entry_Queue``
  * ``No_Task_Hierarchy``
  * ``No_Task_Allocators``
  * ``No_Dynamic_Priorities``
  * ``No_Terminate_Alternatives``
  * ``No_Dynamic_Attachment``
  * ``No_Protected_Type_Allocators``
  * ``No_Local_Protected_Objects``
  * ``No_Requeue_Statements``
  * ``No_Task_Attributes_Package``
  * ``Max_Asynchronous_Select_Nesting =  0``
  * ``Max_Task_Entries =  0``
  * ``Max_Protected_Entries = 1``
  * ``Max_Select_Alternatives = 0``

  This set of restrictions causes the automatic selection of a simplified
  version of the run time that provides improved performance for the
  limited set of tasking functionality permitted by this set of restrictions.

* Pragma Profile (Rational)

  The Rational profile is intended to facilitate porting legacy code that
  compiles with the Rational APEX compiler, even when the code includes non-
  conforming Ada constructs.  The profile enables the following three pragmas:

  * ``pragma Implicit_Packing``
  * ``pragma Overriding_Renamings``
  * ``pragma Use_VADS_Size``


Pragma Profile_Warnings
=======================

Syntax:


.. code-block:: ada

  pragma Profile_Warnings (Ravenscar | Restricted | Rational);


This is an implementation-defined pragma that is similar in
effect to ``pragma Profile`` except that instead of
generating ``Restrictions`` pragmas, it generates
``Restriction_Warnings`` pragmas. The result is that
violations of the profile generate warning messages instead
of error messages.

Pragma Propagate_Exceptions
===========================
.. index:: Interfacing to C++


Syntax:


.. code-block:: ada

  pragma Propagate_Exceptions;


This pragma is now obsolete and, other than generating a warning if warnings
on obsolescent features are enabled, is ignored.
It is retained for compatibility
purposes. It used to be used in connection with optimization of
a now-obsolete mechanism for implementation of exceptions.

Pragma Provide_Shift_Operators
==============================
.. index:: Shift operators


Syntax:


.. code-block:: ada

  pragma Provide_Shift_Operators (integer_first_subtype_LOCAL_NAME);


This pragma can be applied to a first subtype local name that specifies
either an unsigned or signed type. It has the effect of providing the
five shift operators (Shift_Left, Shift_Right, Shift_Right_Arithmetic,
Rotate_Left and Rotate_Right) for the given type. It is similar to
including the function declarations for these five operators, together
with the pragma Import (Intrinsic, ...) statements.

Pragma Psect_Object
===================

Syntax:


::

  pragma Psect_Object (
       [Internal =>] LOCAL_NAME,
    [, [External =>] EXTERNAL_SYMBOL]
    [, [Size     =>] EXTERNAL_SYMBOL]);

  EXTERNAL_SYMBOL ::=
    IDENTIFIER
  | static_string_EXPRESSION


This pragma is identical in effect to pragma ``Common_Object``.

.. _Pragma-Pure_Function:

Pragma Pure_Function
====================

Syntax:


::

  pragma Pure_Function ([Entity =>] function_LOCAL_NAME);


This pragma appears in the same declarative part as a function
declaration (or a set of function declarations if more than one
overloaded declaration exists, in which case the pragma applies
to all entities).  It specifies that the function ``Entity`` is
to be considered pure for the purposes of code generation.  This means
that the compiler can assume that there are no side effects, and
in particular that two calls with identical arguments produce the
same result.  It also means that the function can be used in an
address clause.

Note that, quite deliberately, there are no static checks to try
to ensure that this promise is met, so ``Pure_Function`` can be used
with functions that are conceptually pure, even if they do modify
global variables.  For example, a square root function that is
instrumented to count the number of times it is called is still
conceptually pure, and can still be optimized, even though it
modifies a global variable (the count).  Memo functions are another
example (where a table of previous calls is kept and consulted to
avoid re-computation).

Note also that the normal rules excluding optimization of subprograms
in pure units (when parameter types are descended from System.Address,
or when the full view of a parameter type is limited), do not apply
for the Pure_Function case. If you explicitly specify Pure_Function,
the compiler may optimize away calls with identical arguments, and
if that results in unexpected behavior, the proper action is not to
use the pragma for subprograms that are not (conceptually) pure.

Note: Most functions in a ``Pure`` package are automatically pure, and
there is no need to use pragma ``Pure_Function`` for such functions.  One
exception is any function that has at least one formal of type
``System.Address`` or a type derived from it.  Such functions are not
considered pure by default, since the compiler assumes that the
``Address`` parameter may be functioning as a pointer and that the
referenced data may change even if the address value does not.
Similarly, imported functions are not considered to be pure by default,
since there is no way of checking that they are in fact pure.  The use
of pragma ``Pure_Function`` for such a function will override these default
assumption, and cause the compiler to treat a designated subprogram as pure
in these cases.

Note: If pragma ``Pure_Function`` is applied to a renamed function, it
applies to the underlying renamed function.  This can be used to
disambiguate cases of overloading where some but not all functions
in a set of overloaded functions are to be designated as pure.

If pragma ``Pure_Function`` is applied to a library-level function, the
function is also considered pure from an optimization point of view, but the
unit is not a Pure unit in the categorization sense. So for example, a function
thus marked is free to ``with`` non-pure units.

Pragma Rational
===============

Syntax:


.. code-block:: ada

  pragma Rational;


This pragma is considered obsolescent, but is retained for
compatibility purposes. It is equivalent to:


.. code-block:: ada

  pragma Profile (Rational);


Pragma Ravenscar
================

Syntax:


.. code-block:: ada

  pragma Ravenscar;


This pragma is considered obsolescent, but is retained for
compatibility purposes. It is equivalent to:


.. code-block:: ada

  pragma Profile (Ravenscar);


which is the preferred method of setting the ``Ravenscar`` profile.

.. _Pragma-Refined_Depends:

Pragma Refined_Depends
======================

Syntax:

.. code-block:: ada

  pragma Refined_Depends (DEPENDENCY_RELATION);

  DEPENDENCY_RELATION ::=
       null
    | (DEPENDENCY_CLAUSE {, DEPENDENCY_CLAUSE})

  DEPENDENCY_CLAUSE ::=
      OUTPUT_LIST =>[+] INPUT_LIST
    | NULL_DEPENDENCY_CLAUSE

  NULL_DEPENDENCY_CLAUSE ::= null => INPUT_LIST

  OUTPUT_LIST ::= OUTPUT | (OUTPUT {, OUTPUT})

  INPUT_LIST ::= null | INPUT | (INPUT {, INPUT})

  OUTPUT ::= NAME | FUNCTION_RESULT
  INPUT  ::= NAME

  where FUNCTION_RESULT is a function Result attribute_reference

For the semantics of this pragma, see the entry for aspect ``Refined_Depends`` in
the SPARK 2014 Reference Manual, section 6.1.5.

.. _Pragma-Refined_Global:

Pragma Refined_Global
=====================

Syntax:

.. code-block:: ada

  pragma Refined_Global (GLOBAL_SPECIFICATION);

  GLOBAL_SPECIFICATION ::=
       null
    | (GLOBAL_LIST)
    | (MODED_GLOBAL_LIST {, MODED_GLOBAL_LIST})

  MODED_GLOBAL_LIST ::= MODE_SELECTOR => GLOBAL_LIST

  MODE_SELECTOR ::= In_Out | Input | Output | Proof_In
  GLOBAL_LIST   ::= GLOBAL_ITEM | (GLOBAL_ITEM {, GLOBAL_ITEM})
  GLOBAL_ITEM   ::= NAME

For the semantics of this pragma, see the entry for aspect ``Refined_Global`` in
the SPARK 2014 Reference Manual, section 6.1.4.

.. _Pragma-Refined_Post:

Pragma Refined_Post
===================

Syntax:

.. code-block:: ada

  pragma Refined_Post (boolean_EXPRESSION);

For the semantics of this pragma, see the entry for aspect ``Refined_Post`` in
the SPARK 2014 Reference Manual, section 7.2.7.

.. _Pragma-Refined_State:

Pragma Refined_State
====================

Syntax:

.. code-block:: ada

  pragma Refined_State (REFINEMENT_LIST);

  REFINEMENT_LIST ::=
    (REFINEMENT_CLAUSE {, REFINEMENT_CLAUSE})

  REFINEMENT_CLAUSE ::= state_NAME => CONSTITUENT_LIST

  CONSTITUENT_LIST ::=
       null
    |  CONSTITUENT
    | (CONSTITUENT {, CONSTITUENT})

  CONSTITUENT ::= object_NAME | state_NAME

For the semantics of this pragma, see the entry for aspect ``Refined_State`` in
the SPARK 2014 Reference Manual, section 7.2.2.

Pragma Relative_Deadline
========================

Syntax:


.. code-block:: ada

  pragma Relative_Deadline (time_span_EXPRESSION);


This pragma is standard in Ada 2005, but is available in all earlier
versions of Ada as an implementation-defined pragma.
See Ada 2012 Reference Manual for details.

.. _Pragma-Remote_Access_Type:

Pragma Remote_Access_Type
=========================

Syntax:


::

  pragma Remote_Access_Type ([Entity =>] formal_access_type_LOCAL_NAME);


This pragma appears in the formal part of a generic declaration.
It specifies an exception to the RM rule from E.2.2(17/2), which forbids
the use of a remote access to class-wide type as actual for a formal
access type.

When this pragma applies to a formal access type ``Entity``, that
type is treated as a remote access to class-wide type in the generic.
It must be a formal general access type, and its designated type must
be the class-wide type of a formal tagged limited private type from the
same generic declaration.

In the generic unit, the formal type is subject to all restrictions
pertaining to remote access to class-wide types. At instantiation, the
actual type must be a remote access to class-wide type.

Pragma Rename_Pragma
============================
.. index:: Pragmas, synonyms

Syntax:


::

  pragma Rename_Pragma (
           [New_Name =>] IDENTIFIER,
           [Renamed  =>] pragma_IDENTIFIER);

This pragma provides a mechanism for supplying new names for existing
pragmas. The ``New_Name`` identifier can subsequently be used as a synonym for
the Renamed pragma. For example, suppose you have code that was originally
developed on a compiler that supports Inline_Only as an implementation defined
pragma. And suppose the semantics of pragma Inline_Only are identical to (or at
least very similar to) the GNAT implementation defined pragma
Inline_Always. You could globally replace Inline_Only with Inline_Always.

However, to avoid that source modification, you could instead add a
configuration pragma:

.. code-block:: ada

  pragma Rename_Pragma (
           New_Name => Inline_Only,
           Renamed  => Inline_Always);


Then GNAT will treat "pragma Inline_Only ..." as if you had written
"pragma Inline_Always ...".

Pragma Inline_Only will not necessarily mean the same thing as the other Ada
compiler; it's up to you to make sure the semantics are close enough.

Pragma Restricted_Run_Time
==========================

Syntax:


.. code-block:: ada

  pragma Restricted_Run_Time;


This pragma is considered obsolescent, but is retained for
compatibility purposes. It is equivalent to:


.. code-block:: ada

  pragma Profile (Restricted);


which is the preferred method of setting the restricted run time
profile.

Pragma Restriction_Warnings
===========================

Syntax:


::

  pragma Restriction_Warnings
    (restriction_IDENTIFIER {, restriction_IDENTIFIER});


This pragma allows a series of restriction identifiers to be
specified (the list of allowed identifiers is the same as for
pragma ``Restrictions``). For each of these identifiers
the compiler checks for violations of the restriction, but
generates a warning message rather than an error message
if the restriction is violated.

One use of this is in situations where you want to know
about violations of a restriction, but you want to ignore some of
these violations. Consider this example, where you want to set
Ada_95 mode and enable style checks, but you want to know about
any other use of implementation pragmas:


.. code-block:: ada

  pragma Restriction_Warnings (No_Implementation_Pragmas);
  pragma Warnings (Off, "violation of No_Implementation_Pragmas");
  pragma Ada_95;
  pragma Style_Checks ("2bfhkM160");
  pragma Warnings (On, "violation of No_Implementation_Pragmas");


By including the above lines in a configuration pragmas file,
the Ada_95 and Style_Checks pragmas are accepted without
generating a warning, but any other use of implementation
defined pragmas will cause a warning to be generated.

Pragma Reviewable
=================

Syntax:


.. code-block:: ada

  pragma Reviewable;


This pragma is an RM-defined standard pragma, but has no effect on the
program being compiled, or on the code generated for the program.

To obtain the required output specified in RM H.3.1, the compiler must be
run with various special switches as follows:

* *Where compiler-generated run-time checks remain*

  The switch *-gnatGL*
  may be used to list the expanded code in pseudo-Ada form.
  Runtime checks show up in the listing either as explicit
  checks or operators marked with {} to indicate a check is present.


* *An identification of known exceptions at compile time*

  If the program is compiled with *-gnatwa*,
  the compiler warning messages will indicate all cases where the compiler
  detects that an exception is certain to occur at run time.


* *Possible reads of uninitialized variables*

  The compiler warns of many such cases, but its output is incomplete.

.. only:: PRO or GPL

  The CodePeer analysis tool
  may be used to obtain a comprehensive list of all
  possible points at which uninitialized data may be read.

.. only:: FSF

  A supplemental static analysis tool
  may be used to obtain a comprehensive list of all
  possible points at which uninitialized data may be read.


* *Where run-time support routines are implicitly invoked*

  In the output from *-gnatGL*,
  run-time calls are explicitly listed as calls to the relevant
  run-time routine.


* *Object code listing*

  This may be obtained either by using the *-S* switch,
  or the objdump utility.


* *Constructs known to be erroneous at compile time*

  These are identified by warnings issued by the compiler (use *-gnatwa*).


* *Stack usage information*

  Static stack usage data (maximum per-subprogram) can be obtained via the
  *-fstack-usage* switch to the compiler.
  Dynamic stack usage data (per task) can be obtained via the *-u* switch
  to gnatbind

.. only:: PRO or GPL

  The gnatstack utility
  can be used to provide additional information on stack usage.


* *Object code listing of entire partition*

  This can be obtained by compiling the partition with *-S*,
  or by applying objdump
  to all the object files that are part of the partition.


* *A description of the run-time model*

  The full sources of the run-time are available, and the documentation of
  these routines describes how these run-time routines interface to the
  underlying operating system facilities.


* *Control and data-flow information*

.. only:: PRO or GPL

  The CodePeer tool
  may be used to obtain complete control and data-flow information, as well as
  comprehensive messages identifying possible problems based on this
  information.

.. only:: FSF

  A supplemental static analysis tool
  may be used to obtain complete control and data-flow information, as well as
  comprehensive messages identifying possible problems based on this
  information.

.. _Pragma-Secondary_Stack_Size:

Pragma Secondary_Stack_Size
===========================

Syntax:

.. code-block:: ada

  pragma Secondary_Stack_Size (integer_EXPRESSION);

This pragma appears within the task definition of a single task declaration
or a task type declaration (like pragma ``Storage_Size``) and applies to all
task objects of that type. The argument specifies the size of the secondary
stack to be used by these task objects, and must be of an integer type. The
secondary stack is used to handle functions that return a variable-sized
result, for example a function returning an unconstrained String.

Note this pragma only applies to targets using fixed secondary stacks, like
VxWorks 653 and bare board targets, where a fixed block for the
secondary stack is allocated from the primary stack of the task. By default,
these targets assign a percentage of the primary stack for the secondary stack,
as defined by ``System.Parameter.Sec_Stack_Percentage``. With this pragma,
an ``integer_EXPRESSION`` of bytes is assigned from the primary stack instead.

For most targets, the pragma does not apply as the secondary stack grows on
demand: allocated as a chain of blocks in the heap. The default size of these
blocks can be modified via the :switch:`-D` binder option as described in
:title:`GNAT User's Guide`.

Note that no check is made to see if the secondary stack can fit inside the
primary stack.

Note the pragma cannot appear when the restriction ``No_Secondary_Stack``
is in effect.

Pragma Share_Generic
====================

Syntax:


::

  pragma Share_Generic (GNAME {, GNAME});

  GNAME ::= generic_unit_NAME | generic_instance_NAME


This pragma is provided for compatibility with Dec Ada 83. It has
no effect in GNAT (which does not implement shared generics), other
than to check that the given names are all names of generic units or
generic instances.

.. _Pragma-Shared:

Pragma Shared
=============

This pragma is provided for compatibility with Ada 83. The syntax and
semantics are identical to pragma Atomic.

Pragma Short_Circuit_And_Or
===========================

Syntax:


.. code-block:: ada

  pragma Short_Circuit_And_Or;


This configuration pragma causes any occurrence of the AND operator applied to
operands of type Standard.Boolean to be short-circuited (i.e. the AND operator
is treated as if it were AND THEN). Or is similarly treated as OR ELSE. This
may be useful in the context of certification protocols requiring the use of
short-circuited logical operators. If this configuration pragma occurs locally
within the file being compiled, it applies only to the file being compiled.
There is no requirement that all units in a partition use this option.

Pragma Short_Descriptors
========================

Syntax:


.. code-block:: ada

  pragma Short_Descriptors


This pragma is provided for compatibility with other Ada implementations. It
is recognized but ignored by all current versions of GNAT.

.. _Pragma-Simple_Storage_Pool_Type:

Pragma Simple_Storage_Pool_Type
===============================
.. index:: Storage pool, simple

.. index:: Simple storage pool

Syntax:


.. code-block:: ada

  pragma Simple_Storage_Pool_Type (type_LOCAL_NAME);


A type can be established as a 'simple storage pool type' by applying
the representation pragma ``Simple_Storage_Pool_Type`` to the type.
A type named in the pragma must be a library-level immutably limited record
type or limited tagged type declared immediately within a package declaration.
The type can also be a limited private type whose full type is allowed as
a simple storage pool type.

For a simple storage pool type ``SSP``, nonabstract primitive subprograms
``Allocate``, ``Deallocate``, and ``Storage_Size`` can be declared that
are subtype conformant with the following subprogram declarations:


.. code-block:: ada

  procedure Allocate
    (Pool                     : in out SSP;
     Storage_Address          : out System.Address;
     Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
     Alignment                : System.Storage_Elements.Storage_Count);

  procedure Deallocate
    (Pool : in out SSP;
     Storage_Address          : System.Address;
     Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
     Alignment                : System.Storage_Elements.Storage_Count);

  function Storage_Size (Pool : SSP)
    return System.Storage_Elements.Storage_Count;


Procedure ``Allocate`` must be declared, whereas ``Deallocate`` and
``Storage_Size`` are optional. If ``Deallocate`` is not declared, then
applying an unchecked deallocation has no effect other than to set its actual
parameter to null. If ``Storage_Size`` is not declared, then the
``Storage_Size`` attribute applied to an access type associated with
a pool object of type SSP returns zero. Additional operations can be declared
for a simple storage pool type (such as for supporting a mark/release
storage-management discipline).

An object of a simple storage pool type can be associated with an access
type by specifying the attribute
:ref:`Simple_Storage_Pool <Attribute_Simple_Storage_Pool>`. For example:


.. code-block:: ada

  My_Pool : My_Simple_Storage_Pool_Type;

  type Acc is access My_Data_Type;

  for Acc'Simple_Storage_Pool use My_Pool;



See attribute :ref:`Simple_Storage_Pool <Attribute_Simple_Storage_Pool>`
for further details.

..  _Pragma_Source_File_Name:

Pragma Source_File_Name
=======================

Syntax:


::

  pragma Source_File_Name (
    [Unit_Name   =>] unit_NAME,
    Spec_File_Name =>  STRING_LITERAL,
    [Index => INTEGER_LITERAL]);

  pragma Source_File_Name (
    [Unit_Name   =>] unit_NAME,
    Body_File_Name =>  STRING_LITERAL,
    [Index => INTEGER_LITERAL]);


Use this to override the normal naming convention.  It is a configuration
pragma, and so has the usual applicability of configuration pragmas
(i.e., it applies to either an entire partition, or to all units in a
compilation, or to a single unit, depending on how it is used.
``unit_name`` is mapped to ``file_name_literal``.  The identifier for
the second argument is required, and indicates whether this is the file
name for the spec or for the body.

The optional Index argument should be used when a file contains multiple
units, and when you do not want to use ``gnatchop`` to separate then
into multiple files (which is the recommended procedure to limit the
number of recompilations that are needed when some sources change).
For instance, if the source file :file:`source.ada` contains


.. code-block:: ada

  package B is
  ...
  end B;

  with B;
  procedure A is
  begin
     ..
  end A;


you could use the following configuration pragmas:


.. code-block:: ada

  pragma Source_File_Name
    (B, Spec_File_Name => "source.ada", Index => 1);
  pragma Source_File_Name
    (A, Body_File_Name => "source.ada", Index => 2);


Note that the ``gnatname`` utility can also be used to generate those
configuration pragmas.

Another form of the ``Source_File_Name`` pragma allows
the specification of patterns defining alternative file naming schemes
to apply to all files.


::

  pragma Source_File_Name
    (  [Spec_File_Name  =>] STRING_LITERAL
     [,[Casing          =>] CASING_SPEC]
     [,[Dot_Replacement =>] STRING_LITERAL]);

  pragma Source_File_Name
    (  [Body_File_Name  =>] STRING_LITERAL
     [,[Casing          =>] CASING_SPEC]
     [,[Dot_Replacement =>] STRING_LITERAL]);

  pragma Source_File_Name
    (  [Subunit_File_Name =>] STRING_LITERAL
     [,[Casing            =>] CASING_SPEC]
     [,[Dot_Replacement   =>] STRING_LITERAL]);

  CASING_SPEC ::= Lowercase | Uppercase | Mixedcase


The first argument is a pattern that contains a single asterisk indicating
the point at which the unit name is to be inserted in the pattern string
to form the file name.  The second argument is optional.  If present it
specifies the casing of the unit name in the resulting file name string.
The default is lower case.  Finally the third argument allows for systematic
replacement of any dots in the unit name by the specified string literal.

Note that Source_File_Name pragmas should not be used if you are using
project files. The reason for this rule is that the project manager is not
aware of these pragmas, and so other tools that use the projet file would not
be aware of the intended naming conventions. If you are using project files,
file naming is controlled by Source_File_Name_Project pragmas, which are
usually supplied automatically by the project manager. A pragma
Source_File_Name cannot appear after a :ref:`Pragma_Source_File_Name_Project`.

For more details on the use of the ``Source_File_Name`` pragma, see the
sections on `Using Other File Names` and `Alternative File Naming Schemes`
in the :title:`GNAT User's Guide`.

..  _Pragma_Source_File_Name_Project:

Pragma Source_File_Name_Project
===============================

This pragma has the same syntax and semantics as pragma Source_File_Name.
It is only allowed as a stand-alone configuration pragma.
It cannot appear after a :ref:`Pragma_Source_File_Name`, and
most importantly, once pragma Source_File_Name_Project appears,
no further Source_File_Name pragmas are allowed.

The intention is that Source_File_Name_Project pragmas are always
generated by the Project Manager in a manner consistent with the naming
specified in a project file, and when naming is controlled in this manner,
it is not permissible to attempt to modify this naming scheme using
Source_File_Name or Source_File_Name_Project pragmas (which would not be
known to the project manager).

Pragma Source_Reference
=======================

Syntax:


.. code-block:: ada

  pragma Source_Reference (INTEGER_LITERAL, STRING_LITERAL);


This pragma must appear as the first line of a source file.
``integer_literal`` is the logical line number of the line following
the pragma line (for use in error messages and debugging
information).  ``string_literal`` is a static string constant that
specifies the file name to be used in error messages and debugging
information.  This is most notably used for the output of ``gnatchop``
with the *-r* switch, to make sure that the original unchopped
source file is the one referred to.

The second argument must be a string literal, it cannot be a static
string expression other than a string literal.  This is because its value
is needed for error messages issued by all phases of the compiler.

.. _Pragma-SPARK_Mode:

Pragma SPARK_Mode
=================

Syntax:


::

  pragma SPARK_Mode [(On | Off)] ;


In general a program can have some parts that are in SPARK 2014 (and
follow all the rules in the SPARK Reference Manual), and some parts
that are full Ada 2012.

The SPARK_Mode pragma is used to identify which parts are in SPARK
2014 (by default programs are in full Ada). The SPARK_Mode pragma can
be used in the following places:


*
  As a configuration pragma, in which case it sets the default mode for
  all units compiled with this pragma.

*
  Immediately following a library-level subprogram spec

*
  Immediately within a library-level package body

*
  Immediately following the ``private`` keyword of a library-level
  package spec

*
  Immediately following the ``begin`` keyword of a library-level
  package body

*
  Immediately within a library-level subprogram body


Normally a subprogram or package spec/body inherits the current mode
that is active at the point it is declared. But this can be overridden
by pragma within the spec or body as above.

The basic consistency rule is that you can't turn SPARK_Mode back
``On``, once you have explicitly (with a pragma) turned if
``Off``. So the following rules apply:

If a subprogram spec has SPARK_Mode ``Off``, then the body must
also have SPARK_Mode ``Off``.

For a package, we have four parts:

*
  the package public declarations
*
  the package private part
*
  the body of the package
*
  the elaboration code after ``begin``

For a package, the rule is that if you explicitly turn SPARK_Mode
``Off`` for any part, then all the following parts must have
SPARK_Mode ``Off``. Note that this may require repeating a pragma
SPARK_Mode (``Off``) in the body. For example, if we have a
configuration pragma SPARK_Mode (``On``) that turns the mode on by
default everywhere, and one particular package spec has pragma
SPARK_Mode (``Off``), then that pragma will need to be repeated in
the package body.

Pragma Static_Elaboration_Desired
=================================

Syntax:


.. code-block:: ada

  pragma Static_Elaboration_Desired;


This pragma is used to indicate that the compiler should attempt to initialize
statically the objects declared in the library unit to which the pragma applies,
when these objects are initialized (explicitly or implicitly) by an aggregate.
In the absence of this pragma, aggregates in object declarations are expanded
into assignments and loops, even when the aggregate components are static
constants. When the aggregate is present the compiler builds a static expression
that requires no run-time code, so that the initialized object can be placed in
read-only data space. If the components are not static, or the aggregate has
more that 100 components, the compiler emits a warning that the pragma cannot
be obeyed. (See also the restriction No_Implicit_Loops, which supports static
construction of larger aggregates with static components that include an others
choice.)

Pragma Stream_Convert
=====================

Syntax:


::

  pragma Stream_Convert (
    [Entity =>] type_LOCAL_NAME,
    [Read   =>] function_NAME,
    [Write  =>] function_NAME);


This pragma provides an efficient way of providing user-defined stream
attributes.  Not only is it simpler to use than specifying the attributes
directly, but more importantly, it allows the specification to be made in such
a way that the predefined unit Ada.Streams is not loaded unless it is actually
needed (i.e. unless the stream attributes are actually used); the use of
the Stream_Convert pragma adds no overhead at all, unless the stream
attributes are actually used on the designated type.

The first argument specifies the type for which stream functions are
provided.  The second parameter provides a function used to read values
of this type.  It must name a function whose argument type may be any
subtype, and whose returned type must be the type given as the first
argument to the pragma.

The meaning of the ``Read`` parameter is that if a stream attribute directly
or indirectly specifies reading of the type given as the first parameter,
then a value of the type given as the argument to the Read function is
read from the stream, and then the Read function is used to convert this
to the required target type.

Similarly the ``Write`` parameter specifies how to treat write attributes
that directly or indirectly apply to the type given as the first parameter.
It must have an input parameter of the type specified by the first parameter,
and the return type must be the same as the input type of the Read function.
The effect is to first call the Write function to convert to the given stream
type, and then write the result type to the stream.

The Read and Write functions must not be overloaded subprograms.  If necessary
renamings can be supplied to meet this requirement.
The usage of this attribute is best illustrated by a simple example, taken
from the GNAT implementation of package Ada.Strings.Unbounded:


.. code-block:: ada

  function To_Unbounded (S : String) return Unbounded_String
    renames To_Unbounded_String;

  pragma Stream_Convert
    (Unbounded_String, To_Unbounded, To_String);


The specifications of the referenced functions, as given in the Ada
Reference Manual are:


.. code-block:: ada

  function To_Unbounded_String (Source : String)
    return Unbounded_String;

  function To_String (Source : Unbounded_String)
    return String;


The effect is that if the value of an unbounded string is written to a stream,
then the representation of the item in the stream is in the same format that
would be used for ``Standard.String'Output``, and this same representation
is expected when a value of this type is read from the stream. Note that the
value written always includes the bounds, even for Unbounded_String'Write,
since Unbounded_String is not an array type.

Note that the ``Stream_Convert`` pragma is not effective in the case of
a derived type of a non-limited tagged type. If such a type is specified then
the pragma is silently ignored, and the default implementation of the stream
attributes is used instead.

Pragma Style_Checks
===================

Syntax:


::

  pragma Style_Checks (string_LITERAL | ALL_CHECKS |
                       On | Off [, LOCAL_NAME]);


This pragma is used in conjunction with compiler switches to control the
built in style checking provided by GNAT.  The compiler switches, if set,
provide an initial setting for the switches, and this pragma may be used
to modify these settings, or the settings may be provided entirely by
the use of the pragma.  This pragma can be used anywhere that a pragma
is legal, including use as a configuration pragma (including use in
the :file:`gnat.adc` file).

The form with a string literal specifies which style options are to be
activated.  These are additive, so they apply in addition to any previously
set style check options.  The codes for the options are the same as those
used in the *-gnaty* switch to *gcc* or *gnatmake*.
For example the following two methods can be used to enable
layout checking:

*

  ::

    pragma Style_Checks ("l");


*

  ::

    gcc -c -gnatyl ...


The form ``ALL_CHECKS`` activates all standard checks (its use is equivalent
to the use of the :switch:`gnaty` switch with no options.
See the :title:`GNAT User's Guide` for details.)

Note: the behavior is slightly different in GNAT mode (:switch:`-gnatg` used).
In this case, ``ALL_CHECKS`` implies the standard set of GNAT mode style check
options (i.e. equivalent to :switch:`-gnatyg`).

The forms with ``Off`` and ``On``
can be used to temporarily disable style checks
as shown in the following example:


.. code-block:: ada

  pragma Style_Checks ("k"); -- requires keywords in lower case
  pragma Style_Checks (Off); -- turn off style checks
  NULL;                      -- this will not generate an error message
  pragma Style_Checks (On);  -- turn style checks back on
  NULL;                      -- this will generate an error message


Finally the two argument form is allowed only if the first argument is
``On`` or ``Off``.  The effect is to turn of semantic style checks
for the specified entity, as shown in the following example:


.. code-block:: ada

  pragma Style_Checks ("r"); -- require consistency of identifier casing
  Arg : Integer;
  Rf1 : Integer := ARG;      -- incorrect, wrong case
  pragma Style_Checks (Off, Arg);
  Rf2 : Integer := ARG;      -- OK, no error


Pragma Subtitle
===============

Syntax:


::

  pragma Subtitle ([Subtitle =>] STRING_LITERAL);


This pragma is recognized for compatibility with other Ada compilers
but is ignored by GNAT.

Pragma Suppress
===============

Syntax:


::

  pragma Suppress (Identifier [, [On =>] Name]);


This is a standard pragma, and supports all the check names required in
the RM. It is included here because GNAT recognizes some additional check
names that are implementation defined (as permitted by the RM):


*
  ``Alignment_Check`` can be used to suppress alignment checks
  on addresses used in address clauses. Such checks can also be suppressed
  by suppressing range checks, but the specific use of ``Alignment_Check``
  allows suppression of alignment checks without suppressing other range checks.
  Note that ``Alignment_Check`` is suppressed by default on machines (such as
  the x86) with non-strict alignment.

*
  ``Atomic_Synchronization`` can be used to suppress the special memory
  synchronization instructions that are normally generated for access to
  ``Atomic`` variables to ensure correct synchronization between tasks
  that use such variables for synchronization purposes.

*
  ``Duplicated_Tag_Check`` Can be used to suppress the check that is generated
  for a duplicated tag value when a tagged type is declared.

*
  ``Container_Checks`` Can be used to suppress all checks within Ada.Containers
  and instances of its children, including Tampering_Check.

*
  ``Tampering_Check`` Can be used to suppress tampering check in the containers.

*
  ``Predicate_Check`` can be used to control whether predicate checks are
  active. It is applicable only to predicates for which the policy is
  ``Check``. Unlike ``Assertion_Policy``, which determines if a given
  predicate is ignored or checked for the whole program, the use of
  ``Suppress`` and ``Unsuppress`` with this check name allows a given
  predicate to be turned on and off at specific points in the program.

*
  ``Validity_Check`` can be used specifically to control validity checks.
  If ``Suppress`` is used to suppress validity checks, then no validity
  checks are performed, including those specified by the appropriate compiler
  switch or the ``Validity_Checks`` pragma.

*
  Additional check names previously introduced by use of the ``Check_Name``
  pragma are also allowed.


Note that pragma Suppress gives the compiler permission to omit
checks, but does not require the compiler to omit checks. The compiler
will generate checks if they are essentially free, even when they are
suppressed. In particular, if the compiler can prove that a certain
check will necessarily fail, it will generate code to do an
unconditional 'raise', even if checks are suppressed. The compiler
warns in this case.

Of course, run-time checks are omitted whenever the compiler can prove
that they will not fail, whether or not checks are suppressed.

Pragma Suppress_All
===================

Syntax:


.. code-block:: ada

  pragma Suppress_All;


This pragma can appear anywhere within a unit.
The effect is to apply ``Suppress (All_Checks)`` to the unit
in which it appears.  This pragma is implemented for compatibility with DEC
Ada 83 usage where it appears at the end of a unit, and for compatibility
with Rational Ada, where it appears as a program unit pragma.
The use of the standard Ada pragma ``Suppress (All_Checks)``
as a normal configuration pragma is the preferred usage in GNAT.

.. _Pragma-Suppress_Debug_Info:

Pragma Suppress_Debug_Info
==========================

Syntax:


::

  pragma Suppress_Debug_Info ([Entity =>] LOCAL_NAME);


This pragma can be used to suppress generation of debug information
for the specified entity. It is intended primarily for use in debugging
the debugger, and navigating around debugger problems.

Pragma Suppress_Exception_Locations
===================================

Syntax:


.. code-block:: ada

  pragma Suppress_Exception_Locations;


In normal mode, a raise statement for an exception by default generates
an exception message giving the file name and line number for the location
of the raise. This is useful for debugging and logging purposes, but this
entails extra space for the strings for the messages. The configuration
pragma ``Suppress_Exception_Locations`` can be used to suppress the
generation of these strings, with the result that space is saved, but the
exception message for such raises is null. This configuration pragma may
appear in a global configuration pragma file, or in a specific unit as
usual. It is not required that this pragma be used consistently within
a partition, so it is fine to have some units within a partition compiled
with this pragma and others compiled in normal mode without it.

.. _Pragma-Suppress_Initialization:

Pragma Suppress_Initialization
==============================
.. index:: Suppressing initialization

.. index:: Initialization, suppression of

Syntax:


::

  pragma Suppress_Initialization ([Entity =>] variable_or_subtype_Name);


Here variable_or_subtype_Name is the name introduced by a type declaration
or subtype declaration or the name of a variable introduced by an
object declaration.

In the case of a type or subtype
this pragma suppresses any implicit or explicit initialization
for all variables of the given type or subtype,
including initialization resulting from the use of pragmas
Normalize_Scalars or Initialize_Scalars.

This is considered a representation item, so it cannot be given after
the type is frozen. It applies to all subsequent object declarations,
and also any allocator that creates objects of the type.

If the pragma is given for the first subtype, then it is considered
to apply to the base type and all its subtypes. If the pragma is given
for other than a first subtype, then it applies only to the given subtype.
The pragma may not be given after the type is frozen.

Note that this includes eliminating initialization of discriminants
for discriminated types, and tags for tagged types. In these cases,
you will have to use some non-portable mechanism (e.g. address
overlays or unchecked conversion) to achieve required initialization
of these fields before accessing any object of the corresponding type.

For the variable case, implicit initialization for the named variable
is suppressed, just as though its subtype had been given in a pragma
Suppress_Initialization, as described above.

Pragma Task_Name
================

Syntax


.. code-block:: ada

  pragma Task_Name (string_EXPRESSION);


This pragma appears within a task definition (like pragma
``Priority``) and applies to the task in which it appears.  The
argument must be of type String, and provides a name to be used for
the task instance when the task is created.  Note that this expression
is not required to be static, and in particular, it can contain
references to task discriminants.  This facility can be used to
provide different names for different tasks as they are created,
as illustrated in the example below.

The task name is recorded internally in the run-time structures
and is accessible to tools like the debugger.  In addition the
routine ``Ada.Task_Identification.Image`` will return this
string, with a unique task address appended.


.. code-block:: ada

  --  Example of the use of pragma Task_Name

  with Ada.Task_Identification;
  use Ada.Task_Identification;
  with Text_IO; use Text_IO;
  procedure t3 is

     type Astring is access String;

     task type Task_Typ (Name : access String) is
        pragma Task_Name (Name.all);
     end Task_Typ;

     task body Task_Typ is
        Nam : constant String := Image (Current_Task);
     begin
        Put_Line ("-->" & Nam (1 .. 14) & "<--");
     end Task_Typ;

     type Ptr_Task is access Task_Typ;
     Task_Var : Ptr_Task;

  begin
     Task_Var :=
       new Task_Typ (new String'("This is task 1"));
     Task_Var :=
       new Task_Typ (new String'("This is task 2"));
  end;


Pragma Task_Storage
===================
Syntax:


::

  pragma Task_Storage (
    [Task_Type =>] LOCAL_NAME,
    [Top_Guard =>] static_integer_EXPRESSION);


This pragma specifies the length of the guard area for tasks.  The guard
area is an additional storage area allocated to a task.  A value of zero
means that either no guard area is created or a minimal guard area is
created, depending on the target.  This pragma can appear anywhere a
``Storage_Size`` attribute definition clause is allowed for a task
type.

.. _Pragma-Test_Case:

Pragma Test_Case
================
.. index:: Test cases


Syntax:


::

  pragma Test_Case (
     [Name     =>] static_string_Expression
    ,[Mode     =>] (Nominal | Robustness)
   [, Requires =>  Boolean_Expression]
   [, Ensures  =>  Boolean_Expression]);


The ``Test_Case`` pragma allows defining fine-grain specifications
for use by testing tools.
The compiler checks the validity of the ``Test_Case`` pragma, but its
presence does not lead to any modification of the code generated by the
compiler.

``Test_Case`` pragmas may only appear immediately following the
(separate) declaration of a subprogram in a package declaration, inside
a package spec unit. Only other pragmas may intervene (that is appear
between the subprogram declaration and a test case).

The compiler checks that boolean expressions given in ``Requires`` and
``Ensures`` are valid, where the rules for ``Requires`` are the
same as the rule for an expression in ``Precondition`` and the rules
for ``Ensures`` are the same as the rule for an expression in
``Postcondition``. In particular, attributes ``'Old`` and
``'Result`` can only be used within the ``Ensures``
expression. The following is an example of use within a package spec:


.. code-block:: ada

  package Math_Functions is
     ...
     function Sqrt (Arg : Float) return Float;
     pragma Test_Case (Name     => "Test 1",
                       Mode     => Nominal,
                       Requires => Arg < 10000.0,
                       Ensures  => Sqrt'Result < 10.0);
     ...
  end Math_Functions;


The meaning of a test case is that there is at least one context where
``Requires`` holds such that, if the associated subprogram is executed in
that context, then ``Ensures`` holds when the subprogram returns.
Mode ``Nominal`` indicates that the input context should also satisfy the
precondition of the subprogram, and the output context should also satisfy its
postcondition. Mode ``Robustness`` indicates that the precondition and
postcondition of the subprogram should be ignored for this test case.

.. _Pragma-Thread_Local_Storage:

Pragma Thread_Local_Storage
===========================
.. index:: Task specific storage

.. index:: TLS (Thread Local Storage)

.. index:: Task_Attributes

Syntax:


::

  pragma Thread_Local_Storage ([Entity =>] LOCAL_NAME);


This pragma specifies that the specified entity, which must be
a variable declared in a library-level package, is to be marked as
"Thread Local Storage" (``TLS``). On systems supporting this (which
include Windows, Solaris, GNU/Linux, and VxWorks 6), this causes each
thread (and hence each Ada task) to see a distinct copy of the variable.

The variable must not have default initialization, and if there is
an explicit initialization, it must be either ``null`` for an
access variable, a static expression for a scalar variable, or a fully
static aggregate for a composite type, that is to say, an aggregate all
of whose components are static, and which does not include packed or
discriminated components.

This provides a low-level mechanism similar to that provided by
the ``Ada.Task_Attributes`` package, but much more efficient
and is also useful in writing interface code that will interact
with foreign threads.

If this pragma is used on a system where ``TLS`` is not supported,
then an error message will be generated and the program will be rejected.

Pragma Time_Slice
=================

Syntax:


.. code-block:: ada

  pragma Time_Slice (static_duration_EXPRESSION);


For implementations of GNAT on operating systems where it is possible
to supply a time slice value, this pragma may be used for this purpose.
It is ignored if it is used in a system that does not allow this control,
or if it appears in other than the main program unit.

Pragma Title
============

Syntax:


::

  pragma Title (TITLING_OPTION [, TITLING OPTION]);

  TITLING_OPTION ::=
    [Title    =>] STRING_LITERAL,
  | [Subtitle =>] STRING_LITERAL


Syntax checked but otherwise ignored by GNAT.  This is a listing control
pragma used in DEC Ada 83 implementations to provide a title and/or
subtitle for the program listing.  The program listing generated by GNAT
does not have titles or subtitles.

Unlike other pragmas, the full flexibility of named notation is allowed
for this pragma, i.e., the parameters may be given in any order if named
notation is used, and named and positional notation can be mixed
following the normal rules for procedure calls in Ada.

Pragma Type_Invariant
=====================

Syntax:


::

  pragma Type_Invariant
    ([Entity =>] type_LOCAL_NAME,
     [Check  =>] EXPRESSION);


The ``Type_Invariant`` pragma is intended to be an exact
replacement for the language-defined ``Type_Invariant``
aspect, and shares its restrictions and semantics. It differs
from the language defined ``Invariant`` pragma in that it
does not permit a string parameter, and it is
controlled by the assertion identifier ``Type_Invariant``
rather than ``Invariant``.

.. _Pragma-Type_Invariant_Class:

Pragma Type_Invariant_Class
===========================

Syntax:


::

  pragma Type_Invariant_Class
    ([Entity =>] type_LOCAL_NAME,
     [Check  =>] EXPRESSION);


The ``Type_Invariant_Class`` pragma is intended to be an exact
replacement for the language-defined ``Type_Invariant'Class``
aspect, and shares its restrictions and semantics.

Note: This pragma is called ``Type_Invariant_Class`` rather than
``Type_Invariant'Class`` because the latter would not be strictly
conforming to the allowed syntax for pragmas. The motivation
for providing pragmas equivalent to the aspects is to allow a program
to be written using the pragmas, and then compiled if necessary
using an Ada compiler that does not recognize the pragmas or
aspects, but is prepared to ignore the pragmas. The assertion
policy that controls this pragma is ``Type_Invariant'Class``,
not ``Type_Invariant_Class``.

Pragma Unchecked_Union
======================
.. index:: Unions in C


Syntax:


.. code-block:: ada

  pragma Unchecked_Union (first_subtype_LOCAL_NAME);


This pragma is used to specify a representation of a record type that is
equivalent to a C union. It was introduced as a GNAT implementation defined
pragma in the GNAT Ada 95 mode. Ada 2005 includes an extended version of this
pragma, making it language defined, and GNAT fully implements this extended
version in all language modes (Ada 83, Ada 95, and Ada 2005). For full
details, consult the Ada 2012 Reference Manual, section B.3.3.

Pragma Unevaluated_Use_Of_Old
=============================
.. index:: Attribute Old

.. index:: Attribute Loop_Entry

.. index:: Unevaluated_Use_Of_Old


Syntax:


.. code-block:: ada

  pragma Unevaluated_Use_Of_Old (Error | Warn | Allow);


This pragma controls the processing of attributes Old and Loop_Entry.
If either of these attributes is used in a potentially unevaluated
expression  (e.g. the then or else parts of an if expression), then
normally this usage is considered illegal if the prefix of the attribute
is other than an entity name. The language requires this
behavior for Old, and GNAT copies the same rule for Loop_Entry.

The reason for this rule is that otherwise, we can have a situation
where we save the Old value, and this results in an exception, even
though we might not evaluate the attribute. Consider this example:


.. code-block:: ada

  package UnevalOld is
     K : Character;
     procedure U (A : String; C : Boolean)  -- ERROR
       with Post => (if C then A(1)'Old = K else True);
  end;


If procedure U is called with a string with a lower bound of 2, and
C false, then an exception would be raised trying to evaluate A(1)
on entry even though the value would not be actually used.

Although the rule guarantees against this possibility, it is sometimes
too restrictive. For example if we know that the string has a lower
bound of 1, then we will never raise an exception.
The pragma ``Unevaluated_Use_Of_Old`` can be
used to modify this behavior. If the argument is ``Error`` then an
error is given (this is the default RM behavior). If the argument is
``Warn`` then the usage is allowed as legal but with a warning
that an exception might be raised. If the argument is ``Allow``
then the usage is allowed as legal without generating a warning.

This pragma may appear as a configuration pragma, or in a declarative
part or package specification. In the latter case it applies to
uses up to the end of the corresponding statement sequence or
sequence of package declarations.

Pragma Unimplemented_Unit
=========================

Syntax:


.. code-block:: ada

  pragma Unimplemented_Unit;


If this pragma occurs in a unit that is processed by the compiler, GNAT
aborts with the message :samp:`xxx not implemented`, where
``xxx`` is the name of the current compilation unit.  This pragma is
intended to allow the compiler to handle unimplemented library units in
a clean manner.

The abort only happens if code is being generated.  Thus you can use
specs of unimplemented packages in syntax or semantic checking mode.

.. _Pragma-Universal_Aliasing:

Pragma Universal_Aliasing
=========================

Syntax:


::

  pragma Universal_Aliasing [([Entity =>] type_LOCAL_NAME)];


``type_LOCAL_NAME`` must refer to a type declaration in the current
declarative part.  The effect is to inhibit strict type-based aliasing
optimization for the given type.  In other words, the effect is as though
access types designating this type were subject to pragma No_Strict_Aliasing.
For a detailed description of the strict aliasing optimization, and the
situations in which it must be suppressed, see the section on
``Optimization and Strict Aliasing`` in the :title:`GNAT User's Guide`.

.. _Pragma-Unmodified:

Pragma Unmodified
=================
.. index:: Warnings, unmodified

Syntax:


::

  pragma Unmodified (LOCAL_NAME {, LOCAL_NAME});


This pragma signals that the assignable entities (variables,
``out`` parameters, ``in out`` parameters) whose names are listed are
deliberately not assigned in the current source unit. This
suppresses warnings about the
entities being referenced but not assigned, and in addition a warning will be
generated if one of these entities is in fact assigned in the
same unit as the pragma (or in the corresponding body, or one
of its subunits).

This is particularly useful for clearly signaling that a particular
parameter is not modified, even though the spec suggests that it might
be.

For the variable case, warnings are never given for unreferenced variables
whose name contains one of the substrings
``DISCARD, DUMMY, IGNORE, JUNK, UNUSED`` in any casing. Such names
are typically to be used in cases where such warnings are expected.
Thus it is never necessary to use ``pragma Unmodified`` for such
variables, though it is harmless to do so.

.. _Pragma-Unreferenced:

Pragma Unreferenced
===================
.. index:: Warnings, unreferenced

Syntax:


::

  pragma Unreferenced (LOCAL_NAME {, LOCAL_NAME});
  pragma Unreferenced (library_unit_NAME {, library_unit_NAME});


This pragma signals that the entities whose names are listed are
deliberately not referenced in the current source unit after the
occurrence of the pragma. This
suppresses warnings about the
entities being unreferenced, and in addition a warning will be
generated if one of these entities is in fact subsequently referenced in the
same unit as the pragma (or in the corresponding body, or one
of its subunits).

This is particularly useful for clearly signaling that a particular
parameter is not referenced in some particular subprogram implementation
and that this is deliberate. It can also be useful in the case of
objects declared only for their initialization or finalization side
effects.

If ``LOCAL_NAME`` identifies more than one matching homonym in the
current scope, then the entity most recently declared is the one to which
the pragma applies. Note that in the case of accept formals, the pragma
Unreferenced may appear immediately after the keyword ``do`` which
allows the indication of whether or not accept formals are referenced
or not to be given individually for each accept statement.

The left hand side of an assignment does not count as a reference for the
purpose of this pragma. Thus it is fine to assign to an entity for which
pragma Unreferenced is given.

Note that if a warning is desired for all calls to a given subprogram,
regardless of whether they occur in the same unit as the subprogram
declaration, then this pragma should not be used (calls from another
unit would not be flagged); pragma Obsolescent can be used instead
for this purpose, see :ref:`Pragma_Obsolescent`.

The second form of pragma ``Unreferenced`` is used within a context
clause. In this case the arguments must be unit names of units previously
mentioned in ``with`` clauses (similar to the usage of pragma
``Elaborate_All``. The effect is to suppress warnings about unreferenced
units and unreferenced entities within these units.

For the variable case, warnings are never given for unreferenced variables
whose name contains one of the substrings
``DISCARD, DUMMY, IGNORE, JUNK, UNUSED`` in any casing. Such names
are typically to be used in cases where such warnings are expected.
Thus it is never necessary to use ``pragma Unreferenced`` for such
variables, though it is harmless to do so.

.. _Pragma-Unreferenced_Objects:

Pragma Unreferenced_Objects
===========================
.. index:: Warnings, unreferenced

Syntax:


::

  pragma Unreferenced_Objects (local_subtype_NAME {, local_subtype_NAME});


This pragma signals that for the types or subtypes whose names are
listed, objects which are declared with one of these types or subtypes may
not be referenced, and if no references appear, no warnings are given.

This is particularly useful for objects which are declared solely for their
initialization and finalization effect. Such variables are sometimes referred
to as RAII variables (Resource Acquisition Is Initialization). Using this
pragma on the relevant type (most typically a limited controlled type), the
compiler will automatically suppress unwanted warnings about these variables
not being referenced.

Pragma Unreserve_All_Interrupts
===============================

Syntax:


.. code-block:: ada

  pragma Unreserve_All_Interrupts;


Normally certain interrupts are reserved to the implementation.  Any attempt
to attach an interrupt causes Program_Error to be raised, as described in
RM C.3.2(22).  A typical example is the ``SIGINT`` interrupt used in
many systems for a :kbd:`Ctrl-C` interrupt.  Normally this interrupt is
reserved to the implementation, so that :kbd:`Ctrl-C` can be used to
interrupt execution.

If the pragma ``Unreserve_All_Interrupts`` appears anywhere in any unit in
a program, then all such interrupts are unreserved.  This allows the
program to handle these interrupts, but disables their standard
functions.  For example, if this pragma is used, then pressing
:kbd:`Ctrl-C` will not automatically interrupt execution.  However,
a program can then handle the ``SIGINT`` interrupt as it chooses.

For a full list of the interrupts handled in a specific implementation,
see the source code for the spec of ``Ada.Interrupts.Names`` in
file :file:`a-intnam.ads`.  This is a target dependent file that contains the
list of interrupts recognized for a given target.  The documentation in
this file also specifies what interrupts are affected by the use of
the ``Unreserve_All_Interrupts`` pragma.

For a more general facility for controlling what interrupts can be
handled, see pragma ``Interrupt_State``, which subsumes the functionality
of the ``Unreserve_All_Interrupts`` pragma.

Pragma Unsuppress
=================

Syntax:


::

  pragma Unsuppress (IDENTIFIER [, [On =>] NAME]);


This pragma undoes the effect of a previous pragma ``Suppress``.  If
there is no corresponding pragma ``Suppress`` in effect, it has no
effect.  The range of the effect is the same as for pragma
``Suppress``.  The meaning of the arguments is identical to that used
in pragma ``Suppress``.

One important application is to ensure that checks are on in cases where
code depends on the checks for its correct functioning, so that the code
will compile correctly even if the compiler switches are set to suppress
checks. For example, in a program that depends on external names of tagged
types and wants to ensure that the duplicated tag check occurs even if all
run-time checks are suppressed by a compiler switch, the following
configuration pragma will ensure this test is not suppressed:


.. code-block:: ada

  pragma Unsuppress (Duplicated_Tag_Check);


This pragma is standard in Ada 2005. It is available in all earlier versions
of Ada as an implementation-defined pragma.

Note that in addition to the checks defined in the Ada RM, GNAT recogizes a
number of implementation-defined check names. See the description of pragma
``Suppress`` for full details.

Pragma Use_VADS_Size
====================
.. index:: Size, VADS compatibility

.. index:: Rational profile


Syntax:


.. code-block:: ada

  pragma Use_VADS_Size;


This is a configuration pragma.  In a unit to which it applies, any use
of the 'Size attribute is automatically interpreted as a use of the
'VADS_Size attribute.  Note that this may result in incorrect semantic
processing of valid Ada 95 or Ada 2005 programs.  This is intended to aid in
the handling of existing code which depends on the interpretation of Size
as implemented in the VADS compiler.  See description of the VADS_Size
attribute for further details.

.. _Pragma-Unused:

Pragma Unused
=============
.. index:: Warnings, unused

Syntax:


::

  pragma Unused (LOCAL_NAME {, LOCAL_NAME});


This pragma signals that the assignable entities (variables,
``out`` parameters, and ``in out`` parameters) whose names are listed
deliberately do not get assigned or referenced in the current source unit
after the occurrence of the pragma in the current source unit. This
suppresses warnings about the entities that are unreferenced and/or not
assigned, and, in addition, a warning will be generated if one of these
entities gets assigned or subsequently referenced in the same unit as the
pragma (in the corresponding body or one of its subunits).

This is particularly useful for clearly signaling that a particular
parameter is not modified or referenced, even though the spec suggests
that it might be.

For the variable case, warnings are never given for unreferenced
variables whose name contains one of the substrings
``DISCARD, DUMMY, IGNORE, JUNK, UNUSED`` in any casing. Such names
are typically to be used in cases where such warnings are expected.
Thus it is never necessary to use ``pragma Unmodified`` for such
variables, though it is harmless to do so.

Pragma Validity_Checks
======================

Syntax:


.. code-block:: ada

  pragma Validity_Checks (string_LITERAL | ALL_CHECKS | On | Off);


This pragma is used in conjunction with compiler switches to control the
built-in validity checking provided by GNAT.  The compiler switches, if set
provide an initial setting for the switches, and this pragma may be used
to modify these settings, or the settings may be provided entirely by
the use of the pragma.  This pragma can be used anywhere that a pragma
is legal, including use as a configuration pragma (including use in
the :file:`gnat.adc` file).

The form with a string literal specifies which validity options are to be
activated.  The validity checks are first set to include only the default
reference manual settings, and then a string of letters in the string
specifies the exact set of options required.  The form of this string
is exactly as described for the *-gnatVx* compiler switch (see the
GNAT User's Guide for details).  For example the following two
methods can be used to enable validity checking for mode ``in`` and
``in out`` subprogram parameters:

*

  .. code-block:: ada

    pragma Validity_Checks ("im");


*

  .. code-block:: sh

    $ gcc -c -gnatVim ...


The form ALL_CHECKS activates all standard checks (its use is equivalent
to the use of the :switch:`gnatVa` switch).

The forms with ``Off`` and ``On`` can be used to temporarily disable
validity checks as shown in the following example:


.. code-block:: ada

  pragma Validity_Checks ("c"); -- validity checks for copies
  pragma Validity_Checks (Off); -- turn off validity checks
  A := B;                       -- B will not be validity checked
  pragma Validity_Checks (On);  -- turn validity checks back on
  A := C;                       -- C will be validity checked

.. _Pragma-Volatile:

Pragma Volatile
===============

Syntax:


.. code-block:: ada

  pragma Volatile (LOCAL_NAME);


This pragma is defined by the Ada Reference Manual, and the GNAT
implementation is fully conformant with this definition.  The reason it
is mentioned in this section is that a pragma of the same name was supplied
in some Ada 83 compilers, including DEC Ada 83.  The Ada 95 / Ada 2005
implementation of pragma Volatile is upwards compatible with the
implementation in DEC Ada 83.

.. _Pragma-Volatile_Full_Access:

Pragma Volatile_Full_Access
===========================

Syntax:


.. code-block:: ada

  pragma Volatile_Full_Access (LOCAL_NAME);


This is similar in effect to pragma Volatile, except that any reference to the
object is guaranteed to be done only with instructions that read or write all
the bits of the object. Furthermore, if the object is of a composite type,
then any reference to a subcomponent of the object is guaranteed to read
and/or write all the bits of the object.

The intention is that this be suitable for use with memory-mapped I/O devices
on some machines. Note that there are two important respects in which this is
different from ``pragma Atomic``. First a reference to a ``Volatile_Full_Access``
object is not a sequential action in the RM 9.10 sense and, therefore, does
not create a synchronization point. Second, in the case of ``pragma Atomic``,
there is no guarantee that all the bits will be accessed if the reference
is not to the whole object; the compiler is allowed (and generally will)
access only part of the object in this case.

.. _Pragma-Volatile_Function:

Pragma Volatile_Function
========================

Syntax:

.. code-block:: ada

  pragma Volatile_Function [ (boolean_EXPRESSION) ];

For the semantics of this pragma, see the entry for aspect ``Volatile_Function``
in the SPARK 2014 Reference Manual, section 7.1.2.

Pragma Warning_As_Error
=======================

Syntax:


.. code-block:: ada

  pragma Warning_As_Error (static_string_EXPRESSION);


This configuration pragma allows the programmer to specify a set
of warnings that will be treated as errors. Any warning that
matches the pattern given by the pragma argument will be treated
as an error. This gives more precise control than -gnatwe,
which treats warnings as errors.

This pragma can apply to regular warnings (messages enabled by -gnatw)
and to style warnings (messages that start with "(style)",
enabled by -gnaty).

The pattern may contain asterisks, which match zero or more characters
in the message. For example, you can use ``pragma Warning_As_Error
("bits of*unused")`` to treat the warning message ``warning: 960 bits of
"a" unused`` as an error. All characters other than asterisk are treated
as literal characters in the match. The match is case insensitive; for
example XYZ matches xyz.

Note that the pattern matches if it occurs anywhere within the warning
message string (it is not necessary to put an asterisk at the start and
the end of the message, since this is implied).

Another possibility for the static_string_EXPRESSION which works whether
or not error tags are enabled (*-gnatw.d*) is to use a single
*-gnatw* tag string, enclosed in brackets,
as shown in the example below, to treat one category of warnings as errors.
Note that if you want to treat multiple categories of warnings as errors,
you can use multiple pragma Warning_As_Error.

The above use of patterns to match the message applies only to warning
messages generated by the front end. This pragma can also be applied to
warnings provided by the back end and mentioned in :ref:`Pragma_Warnings`.
By using a single full *-Wxxx* switch in the pragma, such warnings
can also be treated as errors.

The pragma can appear either in a global configuration pragma file
(e.g. :file:`gnat.adc`), or at the start of a file. Given a global
configuration pragma file containing:


.. code-block:: ada

  pragma Warning_As_Error ("[-gnatwj]");


which will treat all obsolescent feature warnings as errors, the
following program compiles as shown (compile options here are
*-gnatwa.d -gnatl -gnatj55*).


::

       1. pragma Warning_As_Error ("*never assigned*");
       2. function Warnerr return String is
       3.    X : Integer;
             |
          >>> error: variable "X" is never read and
              never assigned [-gnatwv] [warning-as-error]

       4.    Y : Integer;
             |
          >>> warning: variable "Y" is assigned but
              never read [-gnatwu]

       5. begin
       6.    Y := 0;
       7.    return %ABC%;
                    |
          >>> error: use of "%" is an obsolescent
              feature (RM J.2(4)), use """ instead
              [-gnatwj] [warning-as-error]

       8. end;

   8 lines: No errors, 3 warnings (2 treated as errors)


Note that this pragma does not affect the set of warnings issued in
any way, it merely changes the effect of a matching warning if one
is produced as a result of other warnings options. As shown in this
example, if the pragma results in a warning being treated as an error,
the tag is changed from "warning:" to "error:" and the string
"[warning-as-error]" is appended to the end of the message.

.. _Pragma_Warnings:

Pragma Warnings
===============

Syntax:


.. code-block:: ada

  pragma Warnings ([TOOL_NAME,] DETAILS [, REASON]);

  DETAILS ::= On | Off
  DETAILS ::= On | Off, local_NAME
  DETAILS ::= static_string_EXPRESSION
  DETAILS ::= On | Off, static_string_EXPRESSION

  TOOL_NAME ::= GNAT | GNATprove

  REASON ::= Reason => STRING_LITERAL {& STRING_LITERAL}

Note: in Ada 83 mode, a string literal may be used in place of a static string
expression (which does not exist in Ada 83).

Note if the second argument of ``DETAILS`` is a ``local_NAME`` then the
second form is always understood. If the intention is to use
the fourth form, then you can write ``NAME & ""`` to force the
intepretation as a *static_string_EXPRESSION*.

Note: if the first argument is a valid ``TOOL_NAME``, it will be interpreted
that way. The use of the ``TOOL_NAME`` argument is relevant only to users
of SPARK and GNATprove, see last part of this section for details.

Normally warnings are enabled, with the output being controlled by
the command line switch.  Warnings (``Off``) turns off generation of
warnings until a Warnings (``On``) is encountered or the end of the
current unit.  If generation of warnings is turned off using this
pragma, then some or all of the warning messages are suppressed,
regardless of the setting of the command line switches.

The ``Reason`` parameter may optionally appear as the last argument
in any of the forms of this pragma. It is intended purely for the
purposes of documenting the reason for the ``Warnings`` pragma.
The compiler will check that the argument is a static string but
otherwise ignore this argument. Other tools may provide specialized
processing for this string.

The form with a single argument (or two arguments if Reason present),
where the first argument is ``ON`` or ``OFF``
may be used as a configuration pragma.

If the ``LOCAL_NAME`` parameter is present, warnings are suppressed for
the specified entity.  This suppression is effective from the point where
it occurs till the end of the extended scope of the variable (similar to
the scope of ``Suppress``). This form cannot be used as a configuration
pragma.

In the case where the first argument is other than ``ON`` or
``OFF``,
the third form with a single static_string_EXPRESSION argument (and possible
reason) provides more precise
control over which warnings are active. The string is a list of letters
specifying which warnings are to be activated and which deactivated. The
code for these letters is the same as the string used in the command
line switch controlling warnings. For a brief summary, use the gnatmake
command with no arguments, which will generate usage information containing
the list of warnings switches supported. For
full details see the section on ``Warning Message Control`` in the
:title:`GNAT User's Guide`.
This form can also be used as a configuration pragma.

The warnings controlled by the :switch:`-gnatw` switch are generated by the
front end of the compiler. The GCC back end can provide additional warnings
and they are controlled by the :switch:`-W` switch. Such warnings can be
identified by the appearance of a string of the form ``[-W{xxx}]`` in the
message which designates the :switch:`-W{xxx}` switch that controls the message.
The form with a single *static_string_EXPRESSION* argument also works for these
warnings, but the string must be a single full :switch:`-W{xxx}` switch in this
case. The above reference lists a few examples of these additional warnings.

The specified warnings will be in effect until the end of the program
or another pragma ``Warnings`` is encountered. The effect of the pragma is
cumulative. Initially the set of warnings is the standard default set
as possibly modified by compiler switches. Then each pragma Warning
modifies this set of warnings as specified. This form of the pragma may
also be used as a configuration pragma.

The fourth form, with an ``On|Off`` parameter and a string, is used to
control individual messages, based on their text. The string argument
is a pattern that is used to match against the text of individual
warning messages (not including the initial "warning: " tag).

The pattern may contain asterisks, which match zero or more characters in
the message. For example, you can use
``pragma Warnings (Off, "bits of*unused")`` to suppress the warning
message ``warning: 960 bits of "a" unused``. No other regular
expression notations are permitted. All characters other than asterisk in
these three specific cases are treated as literal characters in the match.
The match is case insensitive, for example XYZ matches xyz.

Note that the pattern matches if it occurs anywhere within the warning
message string (it is not necessary to put an asterisk at the start and
the end of the message, since this is implied).

The above use of patterns to match the message applies only to warning
messages generated by the front end. This form of the pragma with a string
argument can also be used to control warnings provided by the back end and
mentioned above. By using a single full :switch:`-W{xxx}` switch in the pragma,
such warnings can be turned on and off.

There are two ways to use the pragma in this form. The OFF form can be used
as a configuration pragma. The effect is to suppress all warnings (if any)
that match the pattern string throughout the compilation (or match the
-W switch in the back end case).

The second usage is to suppress a warning locally, and in this case, two
pragmas must appear in sequence:


.. code-block:: ada

  pragma Warnings (Off, Pattern);
  ... code where given warning is to be suppressed
  pragma Warnings (On, Pattern);


In this usage, the pattern string must match in the Off and On
pragmas, and (if *-gnatw.w* is given) at least one matching
warning must be suppressed.

Note: if the ON form is not found, then the effect of the OFF form extends
until the end of the file (pragma Warnings is purely textual, so its effect
does not stop at the end of the enclosing scope).

Note: to write a string that will match any warning, use the string
``"***"``. It will not work to use a single asterisk or two
asterisks since this looks like an operator name. This form with three
asterisks is similar in effect to specifying ``pragma Warnings (Off)`` except (if :switch:`-gnatw.w` is given) that a matching
``pragma Warnings (On, "***")`` will be required. This can be
helpful in avoiding forgetting to turn warnings back on.

Note: the debug flag :switch:`-gnatd.i` can be
used to cause the compiler to entirely ignore all WARNINGS pragmas. This can
be useful in checking whether obsolete pragmas in existing programs are hiding
real problems.

Note: pragma Warnings does not affect the processing of style messages. See
separate entry for pragma Style_Checks for control of style messages.

Users of the formal verification tool GNATprove for the SPARK subset of Ada may
use the version of the pragma with a ``TOOL_NAME`` parameter.

If present, ``TOOL_NAME`` is the name of a tool, currently either ``GNAT`` for the
compiler or ``GNATprove`` for the formal verification tool. A given tool only
takes into account pragma Warnings that do not specify a tool name, or that
specify the matching tool name. This makes it possible to disable warnings
selectively for each tool, and as a consequence to detect useless pragma
Warnings with switch :switch:`-gnatw.w`.

Pragma Weak_External
====================

Syntax:


.. code-block:: ada

  pragma Weak_External ([Entity =>] LOCAL_NAME);


``LOCAL_NAME`` must refer to an object that is declared at the library
level. This pragma specifies that the given entity should be marked as a
weak symbol for the linker. It is equivalent to ``__attribute__((weak))``
in GNU C and causes ``LOCAL_NAME`` to be emitted as a weak symbol instead
of a regular symbol, that is to say a symbol that does not have to be
resolved by the linker if used in conjunction with a pragma Import.

When a weak symbol is not resolved by the linker, its address is set to
zero. This is useful in writing interfaces to external modules that may
or may not be linked in the final executable, for example depending on
configuration settings.

If a program references at run time an entity to which this pragma has been
applied, and the corresponding symbol was not resolved at link time, then
the execution of the program is erroneous. It is not erroneous to take the
Address of such an entity, for example to guard potential references,
as shown in the example below.

Some file formats do not support weak symbols so not all target machines
support this pragma.


.. code-block:: ada

  --  Example of the use of pragma Weak_External

  package External_Module is
    key : Integer;
    pragma Import (C, key);
    pragma Weak_External (key);
    function Present return boolean;
  end External_Module;

  with System; use System;
  package body External_Module is
    function Present return boolean is
    begin
      return key'Address /= System.Null_Address;
    end Present;
  end External_Module;


Pragma Wide_Character_Encoding
==============================

Syntax:


.. code-block:: ada

  pragma Wide_Character_Encoding (IDENTIFIER | CHARACTER_LITERAL);


This pragma specifies the wide character encoding to be used in program
source text appearing subsequently. It is a configuration pragma, but may
also be used at any point that a pragma is allowed, and it is permissible
to have more than one such pragma in a file, allowing multiple encodings
to appear within the same file.

However, note that the pragma cannot immediately precede the relevant
wide character, because then the previous encoding will still be in
effect, causing "illegal character" errors.

The argument can be an identifier or a character literal. In the identifier
case, it is one of ``HEX``, ``UPPER``, ``SHIFT_JIS``,
``EUC``, ``UTF8``, or ``BRACKETS``. In the character literal
case it is correspondingly one of the characters :kbd:`h`, :kbd:`u`,
:kbd:`s`, :kbd:`e`, :kbd:`8`, or :kbd:`b`.

Note that when the pragma is used within a file, it affects only the
encoding within that file, and does not affect withed units, specs,
or subunits.
