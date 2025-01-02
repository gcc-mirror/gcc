------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ U N S T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2014-2025, Free Software Foundation, Inc.         --
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

with Table;
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

   --  However, we also want to be able to interface with back ends that do not
   --  easily handle such uplevel references. One example is the LLVM back end.

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
   --     we can still use the standard GCC back end to build code from it.
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

   --      type AREC1PT is access all AREC1T;

   --      AREC1  : aliased AREC1T;
   --      AREC1P : constant AREC1PT := AREC1'Access;

   --   The fields of AREC1 are set at the point the corresponding entity
   --   is declared (immediately for parameters).

   --   Note: the 1 in all these names is a unique index number. Different
   --   scopes requiring different ARECnT declarations will have different
   --   values of n to ensure uniqueness.

   --   Note: normally the field names in the activation record match the
   --   name of the entity. An exception is when the entity is declared in
   --   a declare block, in which case we append the entity number, to avoid
   --   clashes between the same name declared in different declare blocks.

   --   For all subprograms nested immediately within the corresponding scope,
   --   a parameter AREC1F is passed, and all calls to these routines have
   --   AREC1P added as an additional formal.

   --   Now within the nested procedures, any reference to an uplevel entity
   --   xxx is replaced by typ'Deref(AREC1.xxx) where typ is the type of the
   --   reference.

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
   --          type AREC1PT is access all AREC1T;
   --
   --          AREC1 : aliased AREC1T;
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
   --             Integer'Deref(AREC1F.x) :=
   --               Integer'Deref(AREC1F.rv) + y + b + Integer'Deref(AREC1F.b);
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
   --    case where we can skip this is where the bound is already a constant.
   --    E.g. in the third example above, subtype dynam is expanded as

   --      dynam_LAST : constant Integer := y + 3;
   --      subtype dynam is integer range x .. dynam_LAST;

   --    Now if type dynam is uplevel referenced (as it is in this case), then
   --    the bounds x and dynam_LAST are marked as uplevel references
   --    so that appropriate entries are made in the activation record. Any
   --    explicit reference to such a bound in the front end generated code
   --    will be handled by the normal uplevel reference mechanism which we
   --    described above for case 2. For implicit references by a back end
   --    that needs to unnest things, any such implicit reference to one of
   --    these bounds can be replaced by an appropriate reference to the entry
   --    in the activation record for xx_FIRST or xx_LAST. Thus the back end
   --    can eliminate the problematical uplevel reference without the need to
   --    do the heavy tree modification to do that at the code expansion level.

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
   --  front end, so that we get explicit references to y or dynam_Last. These
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
   --          type AREC1PT is access all AREC1T;
   --
   --          AREC1 : aliased AREC1T;
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
   --             return b in x .. Integer'Deref(AREC1F.dynam_LAST)
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
   --        type AREC1PT is access all AREC1T;
   --
   --        AREC1 : aliased AREC1T;
   --        AREC1P : constant AREC1PT := AREC1'Access;
   --
   --        v1 : integer := x;
   --        AREC1.v1 := v1'Address;
   --
   --        function inner1 (y : integer; AREC1F : AREC1PT) return integer is
   --           type AREC2T is record
   --              AREC1U : AREC1PT;
   --              v2     : Address;
   --           end record;
   --
   --           type AREC2PT is access all AREC2T;
   --
   --           AREC2 : aliased AREC2T;
   --           AREC2P : constant AREC2PT := AREC2'Access;
   --
   --           AREC2.AREC1U := AREC1F;
   --
   --           v2 : integer := Integer'Deref (AREC1F.v1) {+} 1;
   --           AREC2.v2 := v2'Address;
   --
   --           function inner2
   --              (z : integer; AREC2F : AREC2PT) return integer
   --           is
   --           begin
   --              return integer(z {+}
   --                             Integer'Deref (AREC2F.AREC1U.v1) {+}
   --                             Integer'Deref (AREC2F.v2).all);
   --           end inner2;
   --        begin
   --           return integer(y {+}
   --                            inner2 (Integer'Deref (AREC1F.v1), AREC2P));
   --        end inner1;
   --     begin
   --        return inner1 (x, AREC1P);
   --     end case4x;

   --  As can be seen in this example, the index numbers following AREC in the
   --  generated names avoid confusion between AREC names at different levels.

   -------------------------
   -- Name Disambiguation --
   -------------------------

   --  As described above, the translation scheme would raise issues when the
   --  code generator did the actual unnesting if identically named nested
   --  subprograms exist. Similarly overloading would cause a naming issue.

   --  In fact, the expanded code includes qualified names which eliminate this
   --  problem. We omitted the qualification from the expanded examples above
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

   --  In addition, the declarations of ARECnT and ARECnPT get moved to the
   --  outer level when we actually generate C code, so we suffix these names
   --  with the corresponding entity name to make sure they are unique.

   ---------------------------
   -- Terminology for Calls --
   ---------------------------

   --  The level of a subprogram in the nest being analyzed is defined to be
   --  the level of nesting, so the outer level subprogram (the one passed to
   --  Unnest_Subprogram) is 1, subprograms immediately nested within this
   --  outer level subprogram have a level of 2, etc.

   --  Calls within the nest being analyzed are of three types:

   --    Downward call: this is a call from a subprogram to a subprogram that
   --    is immediately nested with in the caller, and thus has a level that
   --    is one greater than the caller. It is a fundamental property of the
   --    nesting structure and visibility that it is not possible to make a
   --    call from level N to level M, where M is greater than N + 1.

   --    Parallel call: this is a call from a nested subprogram to another
   --    nested subprogram that is at the same level.

   --    Upward call: this is a call from a subprogram to a subprogram that
   --    encloses the caller. The level of the callee is less than the level
   --    of the caller, and there is no limit on the difference, e.g. for an
   --    uplevel call, a subprogram at level 5 can call one at level 2 or even
   --    the outer level subprogram at level 1.

   -------------------------------------
   -- Handling of unconstrained types --
   -------------------------------------

   --  Objects whose nominal subtype is an unconstrained array type present
   --  additional complications for translation into LLVM. The address
   --  attribute of such objects points to the first component of the
   --  array, and the bounds are found elsewhere, typically ahead of the
   --  components. In many cases the bounds of an object are stored ahead
   --  of the components and can be retrieved from it. However, if the
   --  object is an expression (e.g. a slice) the bounds are not adjacent
   --  and thus must be conveyed explicitly by means of a so-called
   --  fat pointer. This leads to the following enhancements to the
   --  handling of uplevel references described so far. This applies only
   --  to uplevel references to unconstrained formals of enclosing
   --  subprograms:
   --
   --  a) Uplevel references are detected as before during the tree traversal
   --  in Visit_Node. For reference to uplevel formals, we include those with
   --  an unconstrained array type (e.g. String) even if such a type has
   --  static bounds.
   --
   --  b) references to unconstrained formals are recognized in the Subp
   --  table by means of the predicate Needs_Fat_Pointer.
   --
   --  c) When constructing the required activation record we also construct
   --  a named access type whose designated type is the unconstrained array
   --  type. The activation record of a subprogram that contains such an
   --  uplevel reference includes a component of this access type. The
   --  declaration for that access type is introduced and analyzed before
   --  that of the activation record, so it appears in the subprogram that
   --  has that formal.
   --
   --  d) The uplevel reference is rewritten as an explicit dereference (.all)
   --  of the corresponding pointer component.
   --
   -----------
   -- Subps --
   -----------

   --  Table to record subprograms within the nest being currently analyzed.
   --  Entries in this table are made for each subprogram expanded, and do not
   --  get cleared as we complete the expansion, since we want the table info
   --  around in Cprint for the actual unnesting operation. Subps_First in this
   --  unit records the starting entry in the table for the entries for Subp
   --  and this is also recorded in the Subps_Index field of the outer level
   --  subprogram in the nest. The last subps index for the nest can be found
   --  in the Subp_Entry Last field of this first entry.

   subtype SI_Type is Nat;
   --  Index type for the table

   Subps_First : SI_Type;
   --  Record starting index for entries in the current nest (this is the table
   --  index of the entry for Subp itself, and is recorded in the Subps_Index
   --  field of the entity for this subprogram).

   type Subp_Entry is record
      Ent : Entity_Id;
      --  Entity of the subprogram

      Bod : Node_Id;
      --  Subprogram_Body node for this subprogram

      Lev : Nat;
      --  Subprogram level (1 = outer subprogram (Subp argument), 2 = nested
      --  immediately within this outer subprogram etc.)

      Reachable : Boolean;
      --  This flag is set True if there is a call path from the outer level
      --  subprogram to this subprogram. If Reachable is False, it means that
      --  the subprogram is declared but not actually referenced. We remove
      --  such subprograms from the tree, which simplifies our task, because
      --  we don't have to worry about e.g. uplevel references from such an
      --  unreferenced subprogram, which might require (useless) activation
      --  records to be created. This is computed by setting the outer level
      --  subprogram (Subp itself) as reachable, and then doing a transitive
      --  closure following all calls.

      Uplevel_Ref : Nat;
      --  The outermost level which defines entities which this subprogram
      --  references either directly or indirectly via a call. This cannot
      --  be greater than Lev. If it is equal to Lev, then it means that the
      --  subprogram does not make any uplevel references and that thus it
      --  does not need an activation record pointer passed. If it is less than
      --  Lev, then an activation record pointer is needed, since there is at
      --  least one uplevel reference. This is computed by initially setting
      --  Uplevel_Ref to Lev for all subprograms. Then on the initial tree
      --  traversal, decreasing Uplevel_Ref for an explicit uplevel reference,
      --  and finally by doing a transitive closure that follows calls (if A
      --  calls B and B has an uplevel reference to level X, then A references
      --  level X indirectly).

      Declares_AREC : Boolean;
      --  This is set True for a subprogram which include the declarations
      --  for a local activation record to be passed on downward calls. It
      --  is set True for the target level of an uplevel reference, and for
      --  all intervening nested subprograms. For example, if a subprogram X
      --  at level 5 makes an uplevel reference to an entity declared in a
      --  level 2 subprogram, then the subprograms at levels 4,3,2 enclosing
      --  the level 5 subprogram will have this flag set True.

      Uents : Elist_Id;
      --  This is a list of entities declared in this subprogram which are
      --  uplevel referenced. It contains both objects (which will be put in
      --  the corresponding AREC activation record), and types. The types are
      --  not put in the AREC activation record, but referenced bounds (i.e.
      --  generated _FIRST and _LAST entities, and formal parameters) will be
      --  in the list in their own right.

      Last : SI_Type;
      --  This field is set only in the entry for the outer level subprogram
      --  in a nest, and records the last index in the Subp table for all the
      --  entries for subprograms in this nest.

      ARECnF : Entity_Id;
      --  This entity is defined for all subprograms which need an extra formal
      --  that contains a pointer to the activation record needed for uplevel
      --  references. ARECnF must be defined for any subprogram which has a
      --  direct or indirect uplevel reference (i.e. Reference_Level < Lev).

      ARECn   : Entity_Id;
      ARECnT  : Entity_Id;
      ARECnPT : Entity_Id;
      ARECnP  : Entity_Id;
      --  These AREC entities are defined only for subprograms for which we
      --  generate an activation record declaration, i.e. for subprograms for
      --  which the Declares_AREC flag is set True.

      ARECnU : Entity_Id;
      --  This AREC entity is the uplink component. It is other than Empty only
      --  for nested subprograms that declare an activation record as indicated
      --  by Declares_AREC being True, and which have uplevel references (Lev
      --  greater than Uplevel_Ref). It is the additional component in the
      --  activation record that references the ARECnF pointer (which points
      --  the activation record one level higher, thus forming the chain).

   end record;

   package Subps is new Table.Table (
     Table_Component_Type => Subp_Entry,
     Table_Index_Type     => SI_Type,
     Table_Low_Bound      => 1,
     Table_Initial        => 1000,
     Table_Increment      => 200,
     Table_Name           => "Unnest_Subps");
   --  Records the subprograms in the nest whose outer subprogram is Subp

   -----------------
   -- Subprograms --
   -----------------

   function Get_Level (Subp : Entity_Id; Sub : Entity_Id) return Nat;
   --  Sub is either Subp itself, or a subprogram nested within Subp. This
   --  function returns the level of nesting (Subp = 1, subprograms that
   --  are immediately nested within Subp = 2, etc.).

   function In_Synchronized_Unit (Subp : Entity_Id) return Boolean;
   --  Predicate to identify subprograms declared in task and protected types.
   --  These subprograms are called from outside the compilation and therefore
   --  must be considered reachable (and cannot be eliminated) because we must
   --  generate code for them.

   function Subp_Index (Sub : Entity_Id) return SI_Type;
   --  Given the entity for a subprogram, return corresponding Subp's index

   procedure Unnest_Subprograms (N : Node_Id);
   --  Called to unnest subprograms. If we are in unnest subprogram mode, this
   --  is the call that traverses the tree N and locates all the library-level
   --  subprograms with nested subprograms to process them.

end Exp_Unst;
