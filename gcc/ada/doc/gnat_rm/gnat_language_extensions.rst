.. _GNAT_Language_Extensions:

************************
GNAT language extensions
************************

The GNAT compiler implements a certain number of language extensions on top of
the latest Ada standard, implementing its own extended superset of Ada.

There are two sets of language extensions:

* The first is the curated set. The features in that set are features that we
  consider being worthy additions to the Ada language, and that we want to make
  available to users early on.

* The second is the experimental set. It includes the first, but also
  experimental features, that are here because they're still in an early
  prototyping phase.

How to activate the extended GNAT Ada superset
==============================================

There are two ways to activate the extended GNAT Ada superset:

* The :ref:`Pragma Extensions_Allowed<Pragma_Extensions_Allowed>`. To activate
  the curated set of extensions, you should use

.. code-block:: ada

   pragma Extensions_Allowed (On)

As a configuration pragma, you can either put it at the beginning of a source
file, or in a ``.adc`` file corresponding to your project.

* The ``-gnatX`` option, that you can pass to the compiler directly, will
  activate the curated subset of extensions.

.. attention:: You can activate the extended set of extensions by using either
   the ``-gnatX0`` command line flag, or the pragma ``Extensions_Allowed`` with
   ``All`` as an argument. However, it is not recommended you use this subset
   for serious projects, and is only means as a playground/technology preview.

.. _Curated_Language_Extensions:

Curated Extensions
==================

Local Declarations Without Block
--------------------------------

A basic_declarative_item may appear at the place of any statement.
This avoids the heavy syntax of block_statements just to declare
something locally.

Link to the original RFC:
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-local-vars-without-block.md
For example:

.. code-block:: ada

   if X > 5 then
      X := X + 1;

      Squared : constant Integer := X**2;

      X := X + Squared;
   end if;

Conditional when constructs
---------------------------

This feature extends the use of ``when`` as a way to condition a control-flow
related statement, to all control-flow related statements.

To do a conditional return in a procedure the following syntax should be used:

.. code-block:: ada

   procedure P (Condition : Boolean) is
   begin
      return when Condition;
   end;

This will return from the procedure if ``Condition`` is true.

When being used in a function the conditional part comes after the return value:

.. code-block:: ada

   function Is_Null (I : Integer) return Boolean is
   begin
      return True when I = 0;
      return False;
   end;

In a similar way to the ``exit when`` a ``goto ... when`` can be employed:

.. code-block:: ada

   procedure Low_Level_Optimized is
      Flags : Bitmapping;
   begin
      Do_1 (Flags);
      goto Cleanup when Flags (1);

      Do_2 (Flags);
      goto Cleanup when Flags (32);

      --  ...

   <<Cleanup>>
      --  ...
   end;

.. code-block

To use a conditional raise construct:

.. code-block:: ada

   procedure Foo is
   begin
      raise Error when Imported_C_Func /= 0;
   end;

An exception message can also be added:

.. code-block:: ada

   procedure Foo is
   begin
      raise Error with "Unix Error"
        when Imported_C_Func /= 0;
   end;


Link to the original RFC:
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-conditional-when-constructs.rst

Case pattern matching
---------------------

The selector for a case statement (but not yet for a case expression) may be of a composite type, subject to
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

If ``Caser_1`` is called and both components of X are positive, then
``Do_This`` will be called; otherwise, if either component is nonnegative
then ``Do_That`` will be called; otherwise, ``Do_The_Other_Thing`` will be
called.

In addition, pattern bindings are supported. This is a mechanism
for binding a name to a component of a matching value for use within
an alternative of a case statement. For a component association
that occurs within a case choice, the expression may be followed by
``is <identifier>``. In the special case of a "box" component association,
the identifier may instead be provided within the box. Either of these
indicates that the given identifier denotes (a constant view of) the matching
subcomponent of the case selector.

.. attention:: Binding is not yet supported for arrays or subcomponents
   thereof.

Consider this example (which uses type ``Rec`` from the previous example):

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

This example is the same as the previous one with respect to determining
whether ``Do_This``, ``Do_That``, or ``Do_The_Other_Thing`` will be called. But
for this version, ``Do_This`` takes a parameter and ``Do_That`` takes two
parameters. If ``Do_This`` is called, the actual parameter in the call will be
``X.F1``.

If ``Do_That`` is called, the situation is more complex because there are two
choices for that alternative. If ``Do_That`` is called because the first choice
matched (i.e., because ``X.F1`` is nonnegative and either ``X.F1`` or ``X.F2``
is zero or negative), then the actual parameters of the call will be (in order)
``X.F1`` and ``X.F2``. If ``Do_That`` is called because the second choice
matched (and the first one did not), then the actual parameters will be
reversed.

Within the choice list for single alternative, each choice must define the same
set of bindings and the component subtypes for for a given identifer must all
statically match. Currently, the case of a binding for a nondiscrete component
is not implemented.

If the set of values that match the choice(s) of an earlier alternative
overlaps the corresponding set of a later alternative, then the first set shall
be a proper subset of the second (and the later alternative will not be
executed if the earlier alternative "matches"). All possible values of the
composite type shall be covered. The composite type of the selector shall be an
array or record type that is neither limited nor class-wide. Currently, a "when
others =>" case choice is required; it is intended that this requirement will
be relaxed at some point.

If a subcomponent's subtype does not meet certain restrictions, then the only
value that can be specified for that subcomponent in a case choice expression
is a "box" component association (which matches all possible values for the
subcomponent). This restriction applies if:

- the component subtype is not a record, array, or discrete type; or

- the component subtype is subject to a non-static constraint or has a
  predicate; or:

- the component type is an enumeration type that is subject to an enumeration
  representation clause; or

- the component type is a multidimensional array type or an array type with a
  nonstatic index subtype.

Support for casing on arrays (and on records that contain arrays) is
currently subject to some restrictions. Non-positional
array aggregates are not supported as (or within) case choices. Likewise
for array type and subtype names. The current implementation exceeds
compile-time capacity limits in some annoyingly common scenarios; the
message generated in such cases is usually "Capacity exceeded in compiling
case statement with composite selector type".

Link to the original RFC:
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-pattern-matching.rst

Fixed lower bounds for array types and subtypes
-----------------------------------------------

Unconstrained array types and subtypes can be specified with a lower bound that
is fixed to a certain value, by writing an index range that uses the syntax
``<lower-bound-expression> .. <>``. This guarantees that all objects of the
type or subtype will have the specified lower bound.

For example, a matrix type with fixed lower bounds of zero for each dimension
can be declared by the following:

.. code-block:: ada

    type Matrix is
      array (Natural range 0 .. <>, Natural range 0 .. <>) of Integer;

Objects of type ``Matrix`` declared with an index constraint must have index
ranges starting at zero:

.. code-block:: ada

    M1 : Matrix (0 .. 9, 0 .. 19);
    M2 : Matrix (2 .. 11, 3 .. 22);  -- Warning about bounds; will raise CE

Similarly, a subtype of ``String`` can be declared that specifies the lower
bound of objects of that subtype to be ``1``:

 .. code-block:: ada

    subtype String_1 is String (1 .. <>);

If a string slice is passed to a formal of subtype ``String_1`` in a call to a
subprogram ``S``, the slice's bounds will "slide" so that the lower bound is
``1``.

Within ``S``, the lower bound of the formal is known to be ``1``, so, unlike a
normal unconstrained ``String`` formal, there is no need to worry about
accounting for other possible lower-bound values. Sliding of bounds also occurs
in other contexts, such as for object declarations with an unconstrained
subtype with fixed lower bound, as well as in subtype conversions.

Use of this feature increases safety by simplifying code, and can also improve
the efficiency of indexing operations, since the compiler statically knows the
lower bound of unconstrained array formals when the formal's subtype has index
ranges with static fixed lower bounds.

Link to the original RFC:
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-fixed-lower-bound.rst

Prefixed-view notation for calls to primitive subprograms of untagged types
---------------------------------------------------------------------------

When operating on an untagged type, if it has any primitive operations, and the
first parameter of an operation is of the type (or is an access parameter with
an anonymous type that designates the type), you may invoke these operations
using an ``object.op(...)`` notation, where the parameter that would normally be
the first parameter is brought out front, and the remaining parameters (if any)
appear within parentheses after the name of the primitive operation.

This same notation is already available for tagged types. This extension allows
for untagged types. It is allowed for all primitive operations of the type
independent of whether they were originally declared in a package spec or its
private part, or were inherited and/or overridden as part of a derived type
declaration occuring anywhere, so long as the first parameter is of the type,
or an access parameter designating the type.

For example:

.. code-block:: ada

    generic
       type Elem_Type is private;
    package Vectors is
        type Vector is private;
        procedure Add_Element (V : in out Vector; Elem : Elem_Type);
        function Nth_Element (V : Vector; N : Positive) return Elem_Type;
        function Length (V : Vector) return Natural;
    private
        function Capacity (V : Vector) return Natural;
           --  Return number of elements that may be added without causing
           --  any new allocation of space

        type Vector is ...
          with Type_Invariant => Vector.Length <= Vector.Capacity;
        ...
    end Vectors;

    package Int_Vecs is new Vectors(Integer);

    V : Int_Vecs.Vector;
    ...
    V.Add_Element(42);
    V.Add_Element(-33);

    pragma Assert (V.Length = 2);
    pragma Assert (V.Nth_Element(1) = 42);

Link to the original RFC:
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-prefixed-untagged.rst

Expression defaults for generic formal functions
------------------------------------------------

The declaration of a generic formal function is allowed to specify
an expression as a default, using the syntax of an expression function.

Here is an example of this feature:

.. code-block:: ada

    generic
       type T is private;
       with function Copy (Item : T) return T is (Item); -- Defaults to Item
    package Stacks is

       type Stack is limited private;

       procedure Push (S : in out Stack; X : T); -- Calls Copy on X
       function Pop (S : in out Stack) return T; -- Calls Copy to return item

    private
       -- ...
    end Stacks;

Link to the original RFC:
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-expression-functions-as-default-for-generic-formal-function-parameters.rst

String interpolation
--------------------

The syntax for string literals is extended to support string interpolation.

Within an interpolated string literal, an arbitrary expression, when
enclosed in ``{ ... }``, is expanded at run time into the result of calling
``'Image`` on the result of evaluating the expression enclosed by the brace
characters, unless it is already a string or a single character.

Here is an example of this feature where the expressions ``Name`` and ``X + Y``
will be evaluated and included in the string.

.. code-block:: ada

   procedure Test_Interpolation is
      X    : Integer := 12;
      Y    : Integer := 15;
      Name : String := "Leo";
   begin
      Put_Line (f"The name is {Name} and the sum is {X + Y}.");
   end Test_Interpolation;

In addition, an escape character (``\``) is provided for inserting certain
standard control characters (such as ``\t`` for tabulation or ``\n`` for
newline) or to escape characters with special significance to the
interpolated string syntax, namely ``"``, ``{``, ``}``,and ``\`` itself.

=================   =================
escaped_character   meaning
-----------------   -----------------
``\a``              ALERT
``\b``              BACKSPACE
``\f``              FORM FEED
``\n``              LINE FEED
``\r``              CARRIAGE RETURN
``\t``              CHARACTER TABULATION
``\v``              LINE TABULATION
``\0``              NUL
-----------------   -----------------
``\\``              ``\``
``\"``              ``"``
``\{``              ``{``
``\}``              ``}``
=================   =================

Note that, unlike normal string literals, doubled characters have no
special significance. So to include a double-quote or a brace character
in an interpolated string, they must be preceded by a ``\``.
For example:

.. code-block:: ada

    Put_Line
      (f"X = {X} and Y = {Y} and X+Y = {X+Y};\n" &
       f" a double quote is \" and" &
       f" an open brace is \{");

Finally, a syntax is provided for creating multi-line string literals,
without having to explicitly use an escape sequence such as ``\n``. For
example:

.. code-block:: ada

    Put_Line
      (f"This is a multi-line"
        "string literal"
        "There is no ambiguity about how many"
        "spaces are included in each line");

Here is a link to the original RFC   :
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-string-interpolation.rst

Constrained attribute for generic objects
-----------------------------------------

The ``Constrained`` attribute is permitted for objects of generic types. The
result indicates whether the corresponding actual is constrained.

``Static`` aspect on intrinsic functions
----------------------------------------

The Ada 202x ``Static`` aspect can be specified on Intrinsic imported functions
and the compiler will evaluate some of these intrinsics statically, in
particular the ``Shift_Left`` and ``Shift_Right`` intrinsics.

.. _Experimental_Language_Extensions:

Experimental Language Extensions
================================

Pragma Storage_Model
--------------------

This feature proposes to redesign the concepts of Storage Pools into a more
efficient model allowing higher performances and easier integration with low
footprint embedded run-times.

It also extends it to support distributed memory models, in particular to
support interactions with GPU.

Here is a link to the full RFC:
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-storage-model.rst

Simpler accessibility model
---------------------------

The goal of this feature is to restore a common understanding of accessibility
rules for implementers and users alike. The new rules should both be effective
at preventing errors and feel natural and compatible in an Ada environment
while removing dynamic accessibility checking.

Here is a link to the full RFC:
https://github.com/AdaCore/ada-spark-rfcs/blob/master/prototyped/rfc-simpler-accessibility.md
