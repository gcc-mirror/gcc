------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U N S T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2014-2015, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Expand routines for unnesting subprograms

with Types; use Types;

package Exp_Unst is

   --  -----------------
   --  -- The Problem --
   --  -----------------

   --  Normally, nested subprograms in the source result in corresponding
   --  nested subprograms in the resulting tree. We then expect the back end
   --  to handle such nested subprograms, including all cases of uplevel
   --  references. For example, the GCC back end can do this relatively easily
   --  since GNU C (as an extension) allows nested functions with uplevel
   --  references, and implements an appropriate static chain approach to
   --  dealing with such uplevel references.

   --  However, we also want to be able to interface with back ends that do
   --  not easily handle such uplevel references. One example is the back end
   --  that translates the tree into standard C source code. In the future,
   --  other back ends might need the same capability (e.g. a back end that
   --  generated LLVM intermediate code).

   --  We could imagine simply handling such references in the appropriate
   --  back end. For example the back end that generates C could recognize
   --  nested subprograms and rig up some way of translating them, e.g. by
   --  making a static-link source level visible.

   --  Rather than take that approach, we prefer to do a semantics-preserving
   --  transformation on the GNAT tree, that eliminates the problem before we
   --  hand the tree over to the back end. There are two reasons for preferring
   --  this approach:

   --     First: the work needs only to be done once for all affected back ends
   --     and we can remain within the semantics of the tree. The front end is
   --     full of tree transformations, so we have all the infrastructure for
   --     doing transformations of this type.

   --     Second: given that the transformation will be semantics-preserving,
   --     we can still used the standard GCC back end to build code from it.
   --     This means we can easily run our full test suite to verify that the
   --     transformations are indeed semantics preserving. It is a lot more
   --     work to thoroughly test the output of specialized back ends.

   --  Looking at the problem, we have three situations to deal with. Note
   --  that in these examples, we use all lower case, since that is the way
   --  the internal tree is cased.

   --     First, cases where there are no uplevel references, for example

   --       procedure case1 is
   --          function max (m, n : Integer) return integer is
   --          begin
   --             return integer'max (m, n);
   --          end max;
   --          ...
   --       end case1;

   --     Second, cases where there are explicit uplevel references.

   --       procedure case2 (b : integer) is
   --          procedure Inner (bb : integer);
   --
   --          procedure inner2 is
   --          begin
   --            inner(5);
   --          end;
   --
   --          x  : integer := 77;
   --          y  : constant integer := 15 * 16;
   --          rv : integer := 10;
   --
   --          procedure inner (bb : integer) is
   --          begin
   --             x := rv + y + bb + b;
   --          end;
   --
   --       begin
   --          inner2;
   --       end case2;

   --     In this second example, B, X, RV are uplevel referenced. Y is not
   --     considered as an uplevel reference since it is a static constant
   --     where references are replaced by the value at compile time.

   --   Third, cases where there are implicit uplevel references via types
   --   whose bounds depend on locally declared constants or variables:

   --       function case3 (x, y : integer) return boolean is
   --          subtype dynam is integer range x .. y + 3;
   --          subtype static is integer range 42 .. 73;
   --          xx : dynam := y;
   --
   --          type darr is array (dynam) of Integer;
   --          type darec is record
   --             A : darr;
   --             B : integer;
   --          end record;
   --          darecv : darec;
   --
   --          function inner (b : integer) return boolean is
   --          begin
   --            return b in dynam and then darecv.b in static;
   --          end inner;
   --
   --       begin
   --         return inner (42) and then inner (xx * 3 - y * 2);
   --       end case3;
   --
   --     In this third example, the membership test implicitly references the
   --     the bounds of Dynam, which both involve uplevel references.

   --  ------------------
   --  -- The Solution --
   --  ------------------

   --  Looking at the three cases above, the first case poses no problem at
   --  all. Indeed the subprogram could have been declared at the outer level
   --  (perhaps changing the name). But this style is quite common as a way
   --  of limiting the scope of a local procedure called only within the outer
   --  procedure. We could move it to the outer level (with a name change if
   --  needed), but we don't bother. We leave it nested, and the back end just
   --  translates it as though it were not nested.

   --  In general we leave nested procedures nested, rather than trying to move
   --  them to the outer level (the back end may do that, e.g. as part of the
   --  translation to C, but we don't do it in the tree itself). This saves a
   --  LOT of trouble in terms of visibility and semantics.

   --  But of course we have to deal with the uplevel references. The idea is
   --  to rewrite these nested subprograms so that they no longer have any such
   --  uplevel references, so by the time they reach the back end, they all are
   --  case 1 (no uplevel references) and thus easily handled.

   --  To deal with explicit uplevel references (case 2 above), we proceed with
   --  the following steps:

   --    All entities marked as being uplevel referenced are marked as aliased
   --    since they will be accessed indirectly via an activation record as
   --    described below.

   --    An activation record is created containing system address values
   --    for each uplevel referenced entity in a given scope. In the example
   --    given before, we would have:

   --      type AREC1T is record
   --         b  : Address;
   --         x  : Address;
   --         rv : Address;
   --      end record;

   --      AREC1 : aliased AREC1T;

   --      type AREC1PT is access all AREC1T;
   --      AREC1P : constant AREC1PT := AREC1'Access;

   --   The fields of AREC1 are set at the point the corresponding entity
   --   is declared (immediately for parameters).

   --   Note: the 1 in all these names represents the fact that we are at the
   --   outer level of nesting. As we will see later, deeper levels of nesting
   --   will use AREC2, AREC3, ...

   --   For all subprograms nested immediately within the corresponding scope,
   --   a parameter AREC1F is passed, and all calls to these routines have
   --   AREC1P added as an additional formal.

   --   Now within the nested procedures, any reference to an uplevel entity
   --   xxx is replaced by Tnn!(AREC1.xxx).all (where ! represents a call
   --   to unchecked conversion to convert the address to the access type
   --   and Tnn is a locally declared type that is "access all t", where t
   --   is the type of the reference).

   --   Note: the reason that we use Address as the component type in the
   --   declaration of AREC1T is that we may create this type before we see
   --   the declaration of this type.

   --   The following shows example 2 above after this translation:

   --       procedure case2x (b : aliased Integer) is
   --          type AREC1T is record
   --             b  : Address;
   --             x  : Address;
   --             rv : Address;
   --          end record;
   --
   --          AREC1 : aliased AREC1T;
   --          type AREC1PT is access all AREC1T;
   --          AREC1P : constant AREC1PT := AREC1'Access;
   --
   --          AREC1.b := b'Address;
   --
   --          procedure inner (bb : integer; AREC1F : AREC1PT);
   --
   --          procedure inner2 (AREC1F : AREC1PT) is
   --          begin
   --            inner(5, AREC1F);
   --          end;
   --
   --          x  : aliased integer := 77;
   --          AREC1.x := X'Address;
   --
   --          y  : constant Integer := 15 * 16;
   --
   --          rv : aliased Integer;
   --          AREC1.rv := rv'Address;
   --
   --          procedure inner (bb : integer; AREC1F : AREC1PT) is
   --          begin
   --             type Tnn1 is access all Integer;
   --             type Tnn2 is access all Integer;
   --             type Tnn3 is access all Integer;
   --             Tnn1!(AREC1F.x).all :=
   --               Tnn2!(AREC1F.rv).all + y + b + Tnn3!(AREC1F.b).all;
   --          end;
   --
   --       begin
   --          inner2 (AREC1P);
   --       end case2x;

   --  And now the inner procedures INNER2 and INNER have no uplevel references
   --  so they have been reduced to case 1, which is the case easily handled by
   --  the back end. Note that the generated code is not strictly legal Ada
   --  because of the assignments to AREC1 in the declarative sequence, but the
   --  GNAT tree always allows such mixing of declarations and statements, so
   --  the back end must be prepared to handle this in any case.

   --  Case 3 where we have uplevel references to types is a bit more complex.
   --  That would especially be the case if we did a full transformation that
   --  completely eliminated such uplevel references as we did for case 2. But
   --  instead of trying to do that, we rewrite the subprogram so that the code
   --  generator can easily detect and deal with these uplevel type references.

   --  First we distinguish two cases

   --    Static types are one of the two following cases:

   --      Discrete types whose bounds are known at compile time. This is not
   --      quite the same as what is tested by Is_OK_Static_Subtype, in that
   --      it allows compile time known values that are not static expressions.

   --      Composite types, whose components are (recursively) static types.

   --    Dynamic types are one of the two following cases:

   --      Discrete types with at least one bound not known at compile time.

   --      Composite types with at least one component that is (recursively)
   --      a dynamic type.

   --    Uplevel references to static types are not a problem, the front end
   --    or the code generator fetches the bounds as required, and since they
   --    are compile time known values, this value can just be extracted and
   --    no actual uplevel reference is required.

   --    Uplevel references to dynamic types are a potential problem, since
   --    such references may involve an implicit access to a dynamic bound,
   --    and this reference is an implicit uplevel access.

   --    To fully unnest such references would be messy, since we would have
   --    to create local copies of the dynamic types involved, so that the
   --    front end or code generator could generate an explicit uplevel
   --    reference to the bound involved. Rather than do that, we set things
   --    up so that this situation can be easily detected and dealt with when
   --    there is an implicit reference to the bounds.

   --    What we do is to always generate a local constant for any dynamic
   --    bound in a dynamic subtype xx with name xx_FIRST or xx_LAST. The one
   --    case where we can skip this is where the bound is For
   --    example in the third example above, subtype dynam is expanded as

   --      dynam_LAST  : constant Integer := y + 3;
   --      subtype dynam is integer range x .. dynam_LAST;

   --    Now if type dynam is uplevel referenced (as it is this case), then
   --    the bounds x and dynam_LAST are marked as uplevel references
   --    so that appropriate entries are made in the activation record. Any
   --    explicit reference to such a bound in the front end generated code
   --    will be handled by the normal uplevel reference mechanism which we
   --    described above for case 2. For implicit references by a back end
   --    that needs to unnest things, any such implicit reference to one of
   --    these bounds can be replaced by an appropriate reference to the entry
   --    in the activation record for xx_FIRST or xx_LAST. Thus the back end
   --    can eliminate the problematical uplevel reference without the need to
   --    do the heavy tree modification to do that at the code expansion level

   --  Looking at case 3 again, here is the normal -gnatG expanded code

     --  function case3 (x : integer; y : integer) return boolean is
     --     dynam_LAST : constant integer := y {+} 3;
     --     subtype dynam is integer range x .. dynam_LAST;
     --     subtype static is integer range 42 .. 73;
     --
     --     [constraint_error when
     --       not (y in x .. dynam_LAST)
     --       "range check failed"]
     --
     --     xx : dynam := y;
     --
     --     type darr is array (x .. dynam_LAST) of integer;
     --     type darec is record
     --        a : darr;
     --        b : integer;
     --     end record;
     --     [type TdarrB is array (x .. dynam_LAST range <>) of integer]
     --     freeze TdarrB []
     --     darecv : darec;
     --
     --     function inner (b : integer) return boolean is
     --     begin
     --        return b in x .. dynam_LAST and then darecv.b in 42 .. 73;
     --     end inner;
     --  begin
     --     return inner (42) and then inner (xx {*} 3 {-} y {*} 2);
     --  end case3;

   --  Note: the actual expanded code has fully qualified names so for
   --  example function inner is actually function case3__inner. For now
   --  we ignore that detail to clarify the examples.

   --  Here we see that some of the bounds references are expanded by the
   --  front end, so that we get explicit references to y or dynamLast. These
   --  cases are handled by the normal uplevel reference mechanism described
   --  above for case 2. This is the case for the constraint check for the
   --  initialization of xx, and the range check in function inner.

   --  But the reference darecv.b in the return statement of function
   --  inner has an implicit reference to the bounds of dynam, since to
   --  compute the location of b in the record, we need the length of a.

   --  Here is the full translation of the third example:

   --       function case3x (x, y : integer) return boolean is
   --          type AREC1T is record
   --             x          : Address;
   --             dynam_LAST : Address;
   --          end record;
   --
   --          AREC1 : aliased AREC1T;
   --          type AREC1PT is access all AREC1T;
   --          AREC1P : constant AREC1PT := AREC1'Access;
   --
   --          AREC1.x := x'Address;
   --
   --          dynam_LAST : constant integer := y {+} 3;
   --          AREC1.dynam_LAST := dynam_LAST'Address;
   --          subtype dynam is integer range x .. dynam_LAST;
   --          xx : dynam := y;
   --
   --          [constraint_error when
   --            not (y in x .. dynam_LAST)
   --            "range check failed"]
   --
   --          subtype static is integer range 42 .. 73;
   --
   --          type darr is array (x .. dynam_LAST) of Integer;
   --          type darec is record
   --             A : darr;
   --             B : integer;
   --          end record;
   --          darecv : darec;
   --
   --          function inner (b : integer; AREC1F : AREC1PT) return boolean is
   --          begin
   --             type Tnn is access all Integer
   --             return b in x .. Tnn!(AREC1F.dynam_LAST).all
   --               and then darecv.b in 42 .. 73;
   --          end inner;
   --
   --       begin
   --         return inner (42, AREC1P) and then inner (xx * 3, AREC1P);
   --       end case3x;

   --  And now the back end when it processes darecv.b will access the bounds
   --  of darecv.a by referencing the d and dynam_LAST fields of AREC1P.

   -----------------------------
   -- Multiple Nesting Levels --
   -----------------------------

   --  In our examples so far, we have only nested to a single level, but the
   --  scheme generalizes to multiple levels of nesting and in this section we
   --  discuss how this generalization works.

   --  Consider this example with two nesting levels

   --  To deal with elimination of uplevel references, we follow the same basic
   --  approach described above for case 2, except that we need an activation
   --  record at each nested level. Basically the rule is that any procedure
   --  that has nested procedures needs an activation record. When we do this,
   --  the inner activation records have a pointer (uplink) to the immediately
   --  enclosing activation record, the normal arrangement of static links. The
   --  following shows the full translation of this fourth case.

   --     function case4x (x : integer) return integer is
   --        type AREC1T is record
   --           v1 : Address;
   --        end record;
   --
   --        AREC1 : aliased AREC1T;
   --        type AREC1PT is access all AREC1T;
   --        AREC1P : constant AREC1PT := AREC1'Access;
   --
   --        v1 : integer := x;
   --        AREC1.v1 := v1'Address;
   --
   --        function inner1 (y : integer; AREC1F : AREC1PT) return integer is
   --           type AREC2T is record
   --              AREC1U : AREC1PT := AREC1F;
   --              v2     : Address;
   --           end record;
   --
   --           AREC2 : aliased AREC2T;
   --           type AREC2PT is access all AREC2T;
   --           AREC2P : constant AREC2PT := AREC2'Access;
   --
   --           type Tnn1 is access all Integer;
   --           v2 : integer := Tnn1!(AREC1F.v1).all {+} 1;
   --           AREC2.v2 := v2'Address;
   --
   --           function inner2
   --              (z : integer; AREC2F : AREC2PT) return integer
   --           is
   --           begin
   --              type Tnn1 is access all Integer;
   --              type Tnn2 is access all Integer;
   --              return integer(z {+}
   --                             Tnn1!(AREC2F.AREC1U.v1).all {+}
   --                             Tnn2!(AREC2F.v2).all);
   --           end inner2;
   --        begin
   --           type Tnn is access all Integer;
   --           return integer(y {+} inner2 (Tnn!(AREC1F.v1).all, AREC2P));
   --        end inner1;
   --     begin
   --        return inner1 (x, AREC1P);
   --     end case4x;

   --  As can be seen in this example, the level number following AREC in the
   --  names avoids any confusion between AREC names at different levels.

   -------------------------
   -- Name Disambiguation --
   -------------------------

   --  As described above, the translation scheme would raise issues when the
   --  code generator did the actual unnesting if identically named nested
   --  subprograms exist. Similarly overloading would cause a naming issue.

   --  In fact, the expanded code includes qualified names which eliminate this
   --  problem. We omitted the qualification from the exapnded examples above
   --  for simplicity. But to see this in action, consider this example:

   --    function Mnames return Boolean is
   --       procedure Inner is
   --          procedure Inner is
   --          begin
   --             null;
   --          end;
   --       begin
   --          Inner;
   --       end;
   --       function F (A : Boolean) return Boolean is
   --       begin
   --          return not A;
   --       end;
   --       function F (A : Integer) return Boolean is
   --       begin
   --          return A > 42;
   --       end;
   --    begin
   --       Inner;
   --       return F (42) or F (True);
   --    end;

   --  The expanded code actually looks like:

   --    function mnames return boolean is
   --       procedure mnames__inner is
   --          procedure mnames__inner__inner is
   --          begin
   --             null;
   --             return;
   --          end mnames__inner__inner;
   --       begin
   --          mnames__inner__inner;
   --          return;
   --       end mnames__inner;
   --       function mnames__f (a : boolean) return boolean is
   --       begin
   --          return not a;
   --       end mnames__f;
   --       function mnames__f__2 (a : integer) return boolean is
   --       begin
   --          return a > 42;
   --       end mnames__f__2;
   --    begin
   --       mnames__inner;
   --       return mnames__f__2 (42) or mnames__f (true);
   --    end mnames;

   --  As can be seen from studying this example, the qualification deals both
   --  with the issue of clashing names (mnames__inner, mnames__inner__inner),
   --  and with overloading (mnames__f, mnames__f__2).

   -----------------
   -- Subprograms --
   -----------------

   procedure Check_Uplevel_Reference_To_Type (Typ : Entity_Id);
   --  This procedure is called if Sem_Util.Check_Nested_Access detects an
   --  uplevel reference to a type or subtype entity Typ. On return there are
   --  two cases, if Typ is a static type (defined as a discrete type with
   --  static bounds, or a record all of whose components are of a static type,
   --  or an array whose index and component types are all static types), then
   --  the flag Is_Static_Type (Typ) will be set True, and in this case the
   --  flag Has_Uplevel_Reference is not set since we don't need to worry about
   --  uplevel references to static types. If on the other hand Typ is not a
   --  static type, then the flag Has_Uplevel_Reference will be set, and any
   --  non-static bounds referenced by the type will also be marked as having
   --  uplevel references (by setting Has_Uplevel_Reference for these bounds).

   procedure Note_Uplevel_Reference (N : Node_Id; Subp : Entity_Id);
   --  Called in Unnest_Subprogram_Mode when we detect an explicit uplevel
   --  reference (node N) to an enclosing subprogram Subp.

   procedure Unnest_Subprogram (Subp : Entity_Id; Subp_Body : Node_Id);
   --  Subp is a library level subprogram which has nested subprograms, and
   --  Subp_Body is the corresponding N_Subprogram_Body node. This procedure
   --  declares the AREC types and objects, adds assignments to the AREC record
   --  as required, defines the xxxPTR types for uplevel referenced objects,
   --  adds the ARECP parameter to all nested subprograms which need it, and
   --  modifies all uplevel references appropriately.

end Exp_Unst;
