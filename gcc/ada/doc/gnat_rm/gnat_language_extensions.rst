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

.. attention:: You can activate the experimental set of extensions
   in addition by using either
   the ``-gnatX0`` command line flag, or the pragma ``Extensions_Allowed`` with
   ``All_Extensions`` as an argument. However, it is not recommended you use
   this subset for serious projects; it is only meant as a technology preview
   for use in playground experiments.

.. _Curated_Language_Extensions:

Curated Extensions
==================

Features activated via ``-gnatX`` or
``pragma Extensions_Allowed (On)``.

Local Declarations Without Block
--------------------------------

A ``basic_declarative_item`` may appear at the place of any statement. This
avoids the heavy syntax of block_statements just to declare something locally.

The only valid kind of declarations for now are ``object_declaration``,
``object_renaming_declaration``, ``use_package_clause`` and
``use_type_clause``.

For example:

.. code-block:: ada

   if X > 5 then
      X := X + 1;

      Squared : constant Integer := X**2;

      X := X + Squared;
   end if;

It is generally a good practice to declare local variables (or constants) with as
short a lifetime as possible. However, introducing a declare block to accomplish
this is a relatively heavy syntactic load along with a traditional extra level
of indentation. The alternative syntax supported here allows declaring symbols
in any statement sequence. Lifetime of such local declarations is until the end of
the enclosing construct. The same enclosing construct cannot contain several
declarations of the same symbol; however, overriding symbols from higher-level
scopes works similarly to the explicit ``declare`` block.

If the enclosing construct allows an exception handler (such as an accept
statement, begin-except-end block or a subprogram body), declarations that
appear at the place of a statement are *not* visible within the handler. Only
declarations that precede the beginning of the construct with an exception
handler would be visible in this handler.

.. attention::

  Here are a couple of examples illustrating the scoping rules described above.

   1. Those declarations are not visible from the potential exception handler:

      .. code-block:: ada

         begin
            A : Integer
            ...
         exception
            when others =>
                Put_Line (A'Image) --  ILLEGAL
         end;

   2. The following is legal

      .. code-block:: ada

         declare
            A : Integer := 10;
         begin
            A : Integer := 12;
         end;

      because it is roughly expanded into

      .. code-block:: ada

         declare
            A : Integer := 10;
         begin
            declare
               A : Integer := 12;
            begin
               ...
            end;
         end;

       And as such the second ``A`` declaration is hiding the first one.


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
declaration occurring anywhere, so long as the first parameter is of the type,
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

.. todo::

   I do not understand this feature enough to decide if the description above
   is sufficient for documentation.

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

Constrained attribute for generic objects
-----------------------------------------

The ``Constrained`` attribute is permitted for objects of generic types. The
result indicates whether the corresponding actual is constrained.

``Static`` aspect on intrinsic functions
----------------------------------------

The Ada 202x ``Static`` aspect can be specified on Intrinsic imported functions
and the compiler will evaluate some of these intrinsics statically, in
particular the ``Shift_Left`` and ``Shift_Right`` intrinsics.

First Controlling Parameter
---------------------------

A new pragma/aspect, ``First_Controlling_Parameter``, is introduced for tagged
types, altering the semantics of primitive/controlling parameters. When a
tagged type is marked with this aspect, only subprograms where the first
parameter is of that type will be considered dispatching primitives. This
pragma/aspect applies to the entire hierarchy, starting from the specified
type, without affecting inherited primitives.

Here is an example of this feature:

.. code-block:: ada

    package Example is
       type Root is tagged private;

       procedure P (V : Integer; V2 : Root);
       -- Primitive

       type Child is tagged private
         with First_Controlling_Parameter;

    private
       type Root is tagged null record;
       type Child is new Root with null record;

       overriding
       procedure P (V : Integer; V2 : Child);
       -- Primitive

       procedure P2 (V : Integer; V2 : Child);
       -- NOT Primitive

       function F return Child; -- NOT Primitive

       function F2 (V : Child) return Child;
       -- Primitive, but only controlling on the first parameter
    end;

Note that ``function F2 (V : Child) return Child;`` differs from ``F2 (V : Child)
return Child'Class;`` in that the return type is a specific, definite type. This
is also distinct from the legacy semantics, where further derivations with
added fields would require overriding the function.

The option ``-gnatw_j``, that you can pass to the compiler directly, enables
warnings related to this new language feature. For instance, compiling the
example above without this switch produces no warnings, but compiling it with
``-gnatw_j`` generates the following warning on the declaration of procedure P2:

.. code-block:: ada

    warning: not a dispatching primitive of tagged type "Child"
    warning: disallowed by First_Controlling_Parameter on "Child"

For generic formal tagged types, you can specify whether the type has the
First_Controlling_Parameter aspect enabled:

.. code-block:: ada

    generic
       type T is tagged private with First_Controlling_Parameter;
    package T is
        type U is new T with null record;
        function Foo return U; -- Not a primitive
    end T;

For tagged partial views, the value of the aspect must be consistent between
the partial and full views:

.. code-block:: ada

    package R is
       type T is tagged private;
    ...
    private
       type T is tagged null record with First_Controlling_Parameter; -- ILLEGAL
    end R;

Restricting the position of controlling parameter offers several advantages:

* Simplification of the dispatching rules improves readability of Ada programs.
  One doesn't need to analyze all subprogram parameters to understand if the given
  subprogram is a primitive of a certain tagged type.

* A programmer is free to use any type, including classwide types, on other
  parameters of a subprogram, without the need to consider possible effects of
  overriding a primitive or creating new one.

* Return type of a function is never considered as a controlling parameter.


.. _Experimental_Language_Extensions:

Experimental Language Extensions
================================

Features activated via ``-gnatX0`` or
``pragma Extensions_Allowed (All_Extensions)``.

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

Storage Model
-------------

This extends Storage Pools into a more efficient model allowing higher performances,
easier integration with low footprint embedded run-times and copying data between
different pools of memory. The latter is especially useful when working with distributed
memory models, in particular to support interactions with GPU.

Aspect Storage_Model_Type
^^^^^^^^^^^^^^^^^^^^^^^^^^

A Storage model is a type which is associated with an aspect
"Storage_Model_Type", e.g.:

.. code-block:: Ada

   type A_Model is null record
      with Storage_Model_Type;

Storage_Model_Type itself accepts six parameters:

- Address_Type, the type of the address managed by this model. This has to be
  a scalar type or derived from System.Address.
- Allocate, a procedure used for allocating memory in this model
- Deallocate, a procedure used for deallocating memory in this model
- Copy_To, a procedure used to copy memory from native memory to this model
- Copy_From, a procedure used to copy memory from this model to native memory
- Storage_Size, a function returning the amount of memory left
- Null_Address, a value for the null address value

By default, Address_Type is System.Address, and all other five subprograms are
performing native operations (e.g. the allocator is the native new allocator).
Users can decide to specify one or more of these. When an Address_Type is
specified and different than System.Address, the all other five subprograms have
to be specified.

The prototypes of these procedures are as follows:

.. code-block:: Ada

   procedure Allocate
     (Model           : in out A_Model;
      Storage_Address : out Address_Type;
      Size            : Storage_Count;
      Alignment       : Storage_Count);

   procedure Deallocate
     (Model           : in out A_Model;
      Storage_Address : out Address_Type;
      Size            : Storage_Count;
      Alignment       : Storage_Count);

   procedure Copy_To
     (Model   : in out A_Model;
      Target  : Address_Type;
      Source  : System.Address;
      Size    : Storage_Count);

   procedure Copy_From
     (Model  : in out A_Model;
      Target : System.Address;
      Source : Address_Type;
      Size   : Storage_Count);

   function Storage_Size
     (Pool : A_Model)
      return Storage_Count;

Here's an example of how this could be instantiated in the context of CUDA:

.. code-block:: Ada

   package CUDA_Memory is

      type CUDA_Storage_Model is null record
         with Storage_Model_Type => (
            Address_Type => CUDA_Address,
            Allocate     => CUDA_Allocate,
            Deallocate   => CUDA_Deallocate,
            Copy_To      => CUDA_Copy_To,
            Copy_From    => CUDA_Copy_From,
            Storage_Size => CUDA_Storage_Size,
            Null_Address => CUDA_Null_Address
         );

      type CUDA_Address is new System.Address;
      --  We're assuming for now same address size on host and device

      procedure CUDA_Allocate
        (Model           : in out CUDA_Storage_Model;
         Storage_Address : out CUDA_Address;
         Size            : Storage_Count;
         Alignment       : Storage_Count);

      procedure CUDA_Deallocate
        (Model           : in out CUDA_Storage_Model;
         Storage_Address : out CUDA_Address;
         Size            : Storage_Count;
         Alignment       : Storage_Count);

      procedure CUDA_Copy_To
        (Model  : in out CUDA_Storage_Model;
         Target : CUDA_Address;
         Source : System.Address;
         Size   : Storage_Count);

      procedure CUDA_Copy_From
        (Model   : in out CUDA_Storage_Model;
         Target  : System.Address;
         Source  : CUDA_Address;
         Size    : Storage_Count);

      function CUDA_Storage_Size
        (Pool : CUDA_Storage_Model)
         return Storage_Count return Storage_Count'Last;

      CUDA_Null_Address : constant CUDA_Address :=
         CUDA_Address (System.Null_Address);

      CUDA_Memory : CUDA_Storage_Model;

   end CUDA_Memory;

Aspect Designated_Storage_Model
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

A new aspect, Designated_Storage_Model, allows to specify the memory model
for the objects pointed by an access type. Under this aspect, allocations
and deallocations will come from the specified memory model instead
of the standard ones. In addition, if write operations are needed for
initialization, or if there is a copy of the target object from and to a
standard memory area, the Copy_To and Copy_From functions will be called.
It allows to encompass the capabilities of storage pools, e.g.:

.. code-block:: Ada

   procedure Main is
      type Integer_Array is array (Integer range <>) of Integer;

      type Host_Array_Access is access all Integer_Array;
      type Device_Array_Access is access Integer_Array
         with Designated_Storage_Model => CUDA_Memory;

      procedure Free is new Unchecked_Deallocation
         (Host_Array_Type, Host_Array_Access);
      procedure Free is new Unchecked_Deallocation
         (Device_Array_Type, Device_Array_Access);

      Host_Array : Host_Array_Access := new Integer_Array (1 .. 10);

      Device_Array : Device_Array_Access := new Host_Array (1 .. 10);
      --  Calls CUDA_Storage_Model.Allocate to allocate the fat pointers and
      --  the bounds, then CUDA_Storage_Model.Copy_In to copy the values of the
      --  boundaries.
   begin
      Host_Array.all := (others => 0);

      Device_Array.all := Host_Array.all;
      --  Calls CUDA_Storage_Model.Copy_To to write to the device array from the
      --  native memory.

      Host_Array.all := Device_Array.all;
      --  Calls CUDA_Storage_Model.Copy_From to read from the device array and
      --  write to native memory.

      Free (Host_Array);

      Free (Device_Array);
      --  Calls CUDA_Storage_Model.Deallocate;
   end;

Taking 'Address of an object with a specific memory model returns an object of
the type of the address for that memory category, which may be different from
System.Address.

When copying is performed between two specific memory models, the native memory
is used as a temporary between the two. E.g.:

.. code-block:: Ada

  type Foo_I is access Integer with Designated_Storage_Model => Foo;
  type Bar_I is access Integer with Designated_Storage_Model => Bar;

    X : Foo_I := new Integer;
    Y : Bar_I := new Integer;
  begin
    X.all := Y.all;

conceptually becomes:

.. code-block:: Ada

    X : Foo_I := new Integer;
    T : Integer;
    Y : Bar_I := new Integer;
  begin
    T := Y.all;
    X.all := T;

Legacy Storage Pools
^^^^^^^^^^^^^^^^^^^^^

Legacy Storage Pools are now replaced by a Storage_Model_Type.
They are implemented as follows:

.. code-block:: Ada

   type Root_Storage_Pool is abstract
     new Ada.Finalization.Limited_Controlled with private
   with Storage_Model_Type => (
      Allocate     => Allocate,
      Deallocate   => Deallocate,
      Storage_Size => Storage_Size
   );
   pragma Preelaborable_Initialization (Root_Storage_Pool);

   procedure Allocate
     (Pool                     : in out Root_Storage_Pool;
      Storage_Address          : out System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is abstract;

   procedure Deallocate
     (Pool                     : in out Root_Storage_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : System.Storage_Elements.Storage_Count;
      Alignment                : System.Storage_Elements.Storage_Count)
   is abstract;

   function Storage_Size
     (Pool : Root_Storage_Pool)
      return System.Storage_Elements.Storage_Count
   is abstract;

The legacy notation:

.. code-block:: Ada

   type My_Pools is new Root_Storage_Pool with record [...]

   My_Pool_Instance : Storage_Model_Pool.Storage_Model :=
      My_Pools'(others => <>);

   type Acc is access Integer_Array with Storage_Pool => My_Pool;

can still be accepted as a shortcut for the new syntax.

Attribute Super
---------------
.. index:: Super

The ``Super`` attribute can be applied to objects of tagged types in order
to obtain a view conversion to the most immediate specific parent type.

It cannot be applied to objects of types without any ancestors.

.. code-block:: ada

  type T1 is tagged null record;
  procedure P (V : T1);

  type T2 is new T1 with null record;

  type T3 is new T2 with null record;
  procedure P (V : T3);

  procedure Call (
    V1 : T1'Class;
    V2 : T2'Class;
    V3 : T3'Class) is
  begin
    V1'Super.P; --  Illegal call as T1 doesn't have any ancestors
    V2'Super.P; --  Equivalent to "T1 (V).P;", a non-dispatching call
                --  to T1's primitive procedure P.
    V3'Super.P; --  Equivalent to "T2 (V).P;"; Since T2 doesn't
                --  override P, a non-dispatching call to T1.P is
                --  executed.
  end;

Simpler accessibility model
---------------------------

The goal of this feature is to simplify the accessibility rules by removing
dynamic accessibility checks that are often difficult to understand and debug.
The new rules are effective at preventing errors, at the expense of loosing
some flexibility in the use of anonymous access types.

The feature can be activated with pragma "No_Dynamic_Accessibility_Checks".
As a result, a set of restrictions apply that can be categorized into three
use-case of anonymous access types:

* standalone objects,
* subprogam parameters and
* function results.

Each of those use-cases is explained separately below. All of the refined rules are
compatible with the [use of anonymous access types in SPARK]
(http://docs.adacore.com/spark2014-docs/html/lrm/declarations-and-types.html#access-types).


Standalone objects
^^^^^^^^^^^^^^^^^^

.. code-block:: ada

   Var        : access T := ...
   Var_To_Cst : access constant T := ...
   Cst        : constant access T := ...
   Cst_To_Cst : constant access constant T := ...

The accessibility levels of standalone objects of anonymous access type (both
constants or variables) is derived of the level of their object declaration.
This supports many common use-cases without the employment of ``Unchecked_Access``
while still removing the need for dynamic checks.

The most major benefit of this change is the compatibility with standard Ada rules.

For example, the following assignment is legal without ``Unchecked_Access`` that
would be required without using the No_Dynamic_Accessibility_Checks pragma:

.. code-block:: ada

   pragma Restrictions (No_Dynamic_Accessibility_Checks);

   procedure Accessibility is

      type T is null record;
      type T_Ptr is access all T;

      T_Inst : aliased T;
      Anon  : access T := T_Inst'Access;
      Named : T_Ptr := Anon;

   begin
      null;
   end;

Subprogram parameters
^^^^^^^^^^^^^^^^^^^^^^

.. code-block:: ada

   procedure P (V : access T; X : access constant T);


When the type of a formal parameter is of anonymous access then, from the caller's
perspective, its level is seen to be at least as deep as that of the type of the
corresponding actual parameter (whatever that actual parameter might be) -
meaning any actual can be used for an anonymous access parameter without the use
of 'Unchecked_Access.

.. todo::

   the example below doesn't demonstrate the feature -- X'Access is legal in plain Ada.

.. code-block:: ada

      pragma Restrictions (No_Dynamic_Accessibility_Checks);

      procedure Accessibility is

         procedure Foo (Param : access Integer) is null;
         X : aliased Integer;
      begin
         Foo (X'Access);
      end;

From the callee's perspective, the level of anonymous access formal parameters would be
between the level of the subprogram and the level of the subprogram's locals. This has the effect
of formal parameters being treated as local to the callee except in:

* Use as a function result
* Use as a value for an access discriminant in result object
* Use as an assignments between formal parameters

Note that with these more restricted rules we lose track of accessibility levels when assigned to
local objects thus making (in the example below) the assignment to Node2.Link from Temp below
compile-time illegal.

.. todo::

   the code below gives the same error messages with and without the pragma

.. code-block:: ada

   type Node is record
      Data : Integer;
      Link : access Node;
   end record;

   procedure Swap_Links (Node1, Node2 : in out Node) is
      Temp : constant access Integer := Node1.Link; -- We lose the "association" to Node1
   begin
      Node1.Link := Node2.Link; -- Allowed
      Node2.Link := Temp; -- Not allowed
   end;

   function Identity (N : access Node) return access Node is
      Local : constant access Node := N;
   begin
      if True then
         return N; -- Allowed
      else
         return Local; -- Not allowed
      end if;
   end;


Function results
^^^^^^^^^^^^^^^^

.. code-block:: ada

   function Get (X : Rec) return access T;

.. todo::

   clarify the list/reword

The accessibility level of the result of a call to a function that has an anonymous access result type defined to be as
whatever is deepest out of the following:

* The level of the subprogram
* The level of any actual parameter corresponding to a formal parameter of an anonymous access type
* The level of each parameter that has a part with both one or more access discriminants and an unconstrained subtype
* The level of any actual parameter corresponding to a formal parameter which is explicitly aliased

NOTE: We would need to include an additional item in the list if we were not to enforce the below restriction on tagged types:

* The level of any actual parameter corresponding to a formal parameter of a tagged type

Function result example:

.. todo::

   verify the examples. Clarify, if they define expected behavior with the pragma or general restriction
   that is modified by the pragma

.. code-block:: ada

   declare
      type T is record
         Comp : aliased Integer;
      end record;

      function Identity (Param : access Integer) return access Integer is
      begin
         return Param; -- Legal
      end;

      function Identity_2 (Param : aliased Integer) return access Integer is
      begin
         return Param'Access; -- Legal
      end;

      X : access Integer;
   begin
      X := Identity (X); -- Legal
      declare
         Y : access Integer;
         Z : aliased Integer;
      begin
         X := Identity (Y); -- Illegal since Y is too deep
         X := Identity_2 (Z); -- Illegal since Z is too deep
      end;
   end;

However, an additional restriction that falls out of the above logic is that tagged type extensions *cannot*
allow additional anonymous access discriminants in order to prevent upward conversions potentially making
such "hidden" anonymous access discriminants visible and prone to memory leaks.

.. todo::

   verify the examples. Clarify, if they define expected behavior with the pragma or general restriction
   that is modified by the pragma

Here is an example of one such case of an upward conversion which would lead to a memory leak:

.. code-block:: ada

   declare
      type T is tagged null record;
      type T2 (Disc : access Integer) is new T with null record; -- Must be illegal

      function Identity (Param : aliased T'Class) return access Integer is
      begin
         return T2 (T'Class (Param)).Disc; -- Here P gets effectively returned and set to X
      end;

      X : access Integer;
   begin
      declare
         P : aliased Integer;
         Y : T2 (P'Access);
      begin
         X := Identity (T'Class (Y)); -- Pass local variable P (via Y's discriminant),
                                    -- leading to a memory leak.
      end;
   end;
   ```

   Thus we need to make the following illegal to avoid such situations:

   ```ada
   package Pkg1 is
      type T1 is tagged null record;
      function Func (X1 : T1) return access Integer is (null);
   end;

   package Pkg2 is
      type T2 (Ptr1, Ptr2 : access Integer) is new Pkg1.T1 with null record; -- Illegal
      ...
   end;

In order to prevent upward conversions of anonymous function results (like below), we
also would need to assure that the level of such a result (from the callee's perspective)
is statically deeper:

.. todo::

   verify the examples. Clarify, if they define expected behavior with the pragma or general restriction
   that is modified by the pragma

.. code-block:: ada

   declare
      type Ref is access all Integer;
      Ptr : Ref;
      function Foo (Param : access Integer) return access Integer is
      begin
         return Result : access Integer := Param; do
            Ptr := Ref (Result); -- Not allowed
         end return;
      end;
   begin
      declare
         Local : aliased Integer;
      begin
         Foo (Local'Access).all := 123;
      end;
   end;


Discriminants and allocators
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

.. todo::

   I have removed this section as it was referring to a feature which was never
   implemented by gnat. Double-check that this is correct.

Case pattern matching
---------------------

The selector for a case statement (but not for a case expression) may
be of a composite type, subject to some restrictions (described below).
Aggregate syntax is used for choices of such a case statement; however,
in cases where a "normal" aggregate would require a discrete value, a
discrete subtype may be used instead; box notation can also be used to
match all values.

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
set of bindings and the component subtypes for for a given identifier must all
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

Mutably Tagged Types with Size'Class Aspect
-------------------------------------------

The ``Size'Class`` aspect can be applied to a tagged type to specify a size
constraint for the type and its descendants. When this aspect is specified
on a tagged type, the class-wide type of that type is considered to be a
"mutably tagged" type - meaning that objects of the class-wide type can have
their tag changed by assignment from objects with a different tag.

Example:

.. code-block:: ada

    type Base is tagged null record
        with Size'Class => 16 * 8;  -- Size in bits (128 bits, or 16 bytes)

    type Derived_Type is new Base with record
       Data_Field : Integer;
    end record;  -- ERROR if Derived_Type exceeds 16 bytes

Class-wide types with a specified ``Size'Class`` can be used as the type of
array components, record components, and stand-alone objects.

.. code-block:: ada

    Inst : Base'Class;
    type Array_of_Base is array (Positive range <>) of Base'Class;

If the ``Size'Class`` aspect is specified for a type ``T``, then every
specific descendant of ``T`` [redundant: (including ``T``)]

- shall have a Size that does not exceed the specified value; and

- shall be undiscriminated; and

- shall have no composite subcomponent whose subtype is subject to a
  dynamic constraint; and

- shall have no interface progenitors; and

- shall not have a tagged partial view other than a private extension; and

- shall not have a statically deeper accessibility level than that of ``T``.

In addition to the places where Legality Rules normally apply (see 12.3),
these legality rules apply also in the private part and in the body of an
instance of a generic unit.

For any subtype ``S`` that is a subtype of a descendant of ``T``, ``S'Class'Size`` is
defined to yield the specified value [redundant:,  although ``S'Class'Size`` is
not a static expression].

A class-wide descendant of a type with a specified ``Size'Class`` aspect is
defined to be a "mutably tagged" type. Any subtype of a mutably tagged type is,
by definition, a definite subtype (RM 3.3 notwithstanding). Default
initialization of an object of such a definite subtype proceeds as for the
corresponding specific type, except that ``Program_Error`` is raised if the
specific type is abstract. [In particular, the initial tag of the object is
that of the corresponding specific type.]

An object of a tagged type is defined to be "tag-constrained" if it is

- an object whose type is not mutably tagged; or

- a constant object; or

- a view conversion of a tag-constrained object; or

- a formal ``in out`` or ``out`` parameter whose corresponding
  actual parameter is tag-constrained.

In the case of an assignment to a tagged variable that
is not tag-constrained, no check is performed that the tag of the value of
the expression is the same as that of the target (RM 5.2 notwithstanding).
Instead, the tag of the target object becomes that of the source object of
the assignment.
An assignment to a composite object similarly copies the tags of any
sub-components of the source object that have a mutably-tagged type.

The ``Constrained`` attribute is defined for any name denoting an object of a
mutably tagged type (RM 3.7.2 notwithstanding). In this case, the Constrained
attribute yields the value True if the object is tag-constrained and False
otherwise.

Renaming is not allowed (see 8.5.1) for a type conversion having an operand of
a mutably tagged type ``MT`` and a target type ``TT`` such that ``TT'Class``
does not cover ``MT``, nor for any part of such an object, nor for any slice
of such an object. This rule also applies in any context where a name is
required to be one for which "renaming is allowed" (for example, see RM 12.4).

A name denoting a view of a variable of a mutably tagged type shall not
occur as an operative constituent of the prefix of a name denoting a
prefixed view of a callable entity, except as the callee name in a call to
the callable entity.

For a type conversion between two general access types, either both or neither
of the designated types shall be mutably tagged. For an ``Access`` (or
``Unchecked_Access``) attribute reference, the designated type of the type of the
attribute reference and the type of the prefix of the attribute shall either
both or neither be mutably tagged.

The execution of a construct is erroneous if the construct has a constituent
that is a name denoting a sub-component of a tagged object and the object's
tag is changed by this execution between evaluating the name and the last use
(within this execution) of the subcomponent denoted by the name.

If the type of a formal parameter is a specific tagged type then the execution
of the call is erroneous if the tag of the actual is changed while the formal
parameter exists (that is, before leaving the corresponding callable
construct).

Generalized Finalization
------------------------

The ``Finalizable`` aspect can be applied to any record type, tagged or not,
to specify that it provides the same level of control on the operations of
initialization, finalization, and assignment of objects as the controlled
types (see RM 7.6(2) for a high-level overview). The only restriction is
that the record type must be a root type, in other words not a derived type.

The aspect additionally makes it possible to specify relaxed semantics for
the finalization operations by means of the ``Relaxed_Finalization`` setting.

Example:

.. code-block:: ada

    type Ctrl is record
      Id : Natural := 0;
    end record
      with Finalizable => (Initialize           => Initialize,
                           Adjust               => Adjust,
                           Finalize             => Finalize,
                           Relaxed_Finalization => True);

    procedure Adjust     (Obj : in out Ctrl);
    procedure Finalize   (Obj : in out Ctrl);
    procedure Initialize (Obj : in out Ctrl);

The three procedures have the same profile, taking a single ``in out T``
parameter.

We follow the same dynamic semantics as controlled objects:

 - ``Initialize`` is called when an object of type ``T`` is declared without
   default expression.

 - ``Adjust`` is called after an object of type ``T`` is assigned a new value.

 - ``Finalize`` is called when an object of type ``T`` goes out of scope (for
   stack-allocated objects) or is explicitly deallocated (for heap-allocated
   objects). It is also called when on the value being replaced in an
   assignment.

However the following differences are enforced by default when compared to the
current Ada controlled-objects finalization model:

* No automatic finalization of heap allocated objects: ``Finalize`` is only
  called when an object is implicitly deallocated. As a consequence, no-runtime
  support is needed for the implicit case, and no header will be maintained for
  this in heap-allocated controlled objects.

  Heap-allocated objects allocated through a nested access type definition will
  hence **not** be deallocated either. The result is simply that memory will be
  leaked in those cases.

* The ``Finalize`` procedure should have have the :ref:`No_Raise_Aspect` specified.
  If that's not the case, a compilation error will be raised.

Additionally, two other configuration aspects are added,
``Legacy_Heap_Finalization`` and ``Exceptions_In_Finalize``:

* ``Legacy_Heap_Finalization``: Uses the legacy automatic finalization of
  heap-allocated objects

* ``Exceptions_In_Finalize``: Allow users to have a finalizer that raises exceptions
  **NB!** note that using this aspect introduces execution time penalities.

.. _No_Raise_Aspect:

No_Raise aspect
----------------

The ``No_Raise`` aspect can be applied to a subprogram to declare that this subprogram is not
expected to raise any exceptions. Should an exception still occur during the execution of
this subpropgram, ``Program_Error`` is raised.

New specification for ``Ada.Finalization.Controlled``
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

``Ada.Finalization.Controlled`` is now specified as:

.. code-block:: ada

   type Controlled is abstract tagged null record
      with Initialize => Initialize,
         Adjust => Adjust,
         Finalize => Finalize,
         Legacy_Heap_Finalization, Exceptions_In_Finalize;

         procedure Initialize (Self : in out Controlled) is abstract;
         procedure Adjust (Self : in out Controlled) is abstract;
         procedure Finalize (Self : in out Controlled) is abstract;


### Examples

A simple example of a ref-counted type:

.. code-block:: ada

   type T is record
      Value : Integer;
      Ref_Count : Natural := 0;
   end record;

   procedure Inc_Ref (X : in out T);
   procedure Dec_Ref (X : in out T);

   type T_Access is access all T;

   type T_Ref is record
      Value : T_Access;
   end record
      with Adjust   => Adjust,
         Finalize => Finalize;

   procedure Adjust (Ref : in out T_Ref) is
   begin
      Inc_Ref (Ref.Value);
   end Adjust;

   procedure Finalize (Ref : in out T_Ref) is
   begin
      Def_Ref (Ref.Value);
   end Finalize;


A simple file handle that ensures resources are properly released:

.. code-block:: ada

   package P is
      type File (<>) is limited private;

      function Open (Path : String) return File;

      procedure Close (F : in out File);
   private
      type File is limited record
         Handle : ...;
      end record
         with Finalize => Close;


Finalized tagged types
^^^^^^^^^^^^^^^^^^^^^^^

Aspects are inherited by derived types and optionally overriden by those. The
compiler-generated calls to the user-defined operations are then
dispatching whenever it makes sense, i.e. the object in question is of
classwide type and the class includes at least one finalized-type.

However note that for simplicity, it is forbidden to change the value of any of
those new aspects in derived types.

Composite types
^^^^^^^^^^^^^^^

When a finalized type is used as a component of a composite type, the latter
becomes finalized as well. The three primitives are derived automatically
in order to call the primitives of their components.

If that composite type was already user-finalized, then the compiler
calls the primitives of the components so as to stay consistent with today's
controlled types's behavior.

So, ``Initialize`` and ``Adjust`` are called on components before they
are called on the composite object, but ``Finalize`` is  called on the composite
object first.

Interoperability with controlled types
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

As a consequence of the redefinition of the ``Controlled`` type as a base type
with the new aspects defined, interoperability with controlled type naturally
follows the definition of the above rules. In particular:

* It is possible to have a new finalized type have a controlled type
  component
* It is possible to have a controlled type have a finalized type
  component


Inference of Dependent Types in Generic Instantiations
------------------------------------------------------

If a generic formal type T2 depends on another formal type T1,
the actual for T1 can be inferred from the actual for T2.
That is, you can give the actual for T2, and leave out the one
for T1.

For example, ``Ada.Unchecked_Deallocation`` has two generic formals:

.. code-block:: ada

   generic
      type Object (<>) is limited private;
      type Name is access Object;
   procedure Ada.Unchecked_Deallocation (X : in out Name);

where ``Name`` depends on ``Object``. With this language extension,
you can leave out the actual for ``Object``, as in:

.. code-block:: ada

   type Integer_Access is access all Integer;

   procedure Free is new Unchecked_Deallocation (Name => Integer_Access);

The compiler will infer that the actual type for ``Object`` is ``Integer``.
Note that named notation is always required when using inference.

The following inferences are allowed:

- For a formal access type, the designated type can be inferred.

- For a formal array type, the index type(s) and the component
  type can be inferred.

- For a formal type with discriminants, the type(s) of the discriminants
  can be inferred.

Example for arrays:

.. code-block:: ada

   generic
      type Element_Type is private;
      type Index_Type is (<>);
      type Array_Type is array (Index_Type range <>) of Element_Type;
   package Array_Operations is
      ...
   end Array_Operations;

   ...

   type Int_Array is array (Positive range <>) of Integer;

   package Int_Array_Operations is new Array_Operations (Array_Type => Int_Array);

The index and component types of ``Array_Type`` are inferred from
``Int_Array``, so that the above instantiation is equivalent to
the following standard-Ada instantiation:

.. code-block:: ada

   package Int_Array_Operations is new Array_Operations
      (Element_Type => Integer,
         Index_Type   => Positive,
         Array_Type   => Int_Array);


External_Initialization Aspect
------------------------------

The ``External_Initialization`` aspect provides a feature similar to Rust's ``include_bytes!``
and to C23's ``#embed``. It has the effect of initializing an object with the contents of
a file specified by a file path.

Only string objects and objects of type ``Ada.Streams.Stream_Element_Array`` can be subject
to the ``External_Initialization`` aspect.

Example:

.. code-block:: ada

   with Ada.Streams;

   package P is
      S : constant String with External_Initialization => "foo.txt";

      X : constant Ada.Streams.Stream_Element_Array with External_Initialization => "bar.bin";
   end P;

``External_Initialization`` aspect accepts the following parameters:

- mandatory ``Path``: the path the compiler uses to access the binary resource;
- optional ``Maximum_Size``: the maximum number of bytes the compiler reads from
  the resource;
- optional ``If_Empty``: an expression used in place of read data in case
  the resource is empty;

``Path`` is resolved according to the same rules the compiler uses for loading the source files.

.. attention:: The maximum size of loaded files is limited to 2\ :sup:`31` bytes.
