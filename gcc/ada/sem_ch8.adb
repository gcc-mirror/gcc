------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Exp_Disp;       use Exp_Disp;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Freeze;         use Freeze;
with Ghost;          use Ghost;
with Impunit;        use Impunit;
with Lib;            use Lib;
with Lib.Load;       use Lib.Load;
with Lib.Xref;       use Lib.Xref;
with Local_Restrict;
with Mutably_Tagged; use Mutably_Tagged;
with Namet;          use Namet;
with Namet.Sp;       use Namet.Sp;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Output;         use Output;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Cat;        use Sem_Cat;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch4;        use Sem_Ch4;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch10;       use Sem_Ch10;
with Sem_Ch12;       use Sem_Ch12;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Dim;        use Sem_Dim;
with Sem_Disp;       use Sem_Disp;
with Sem_Dist;       use Sem_Dist;
with Sem_Elab;       use Sem_Elab;
with Sem_Eval;       use Sem_Eval;
with Sem_Prag;       use Sem_Prag;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sem_Type;       use Sem_Type;
with Stand;          use Stand;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinfo.CN;       use Sinfo.CN;
with Snames;         use Snames;
with Style;
with Table;
with Tbuild;         use Tbuild;
with Uintp;          use Uintp;
with Warnsw;         use Warnsw;

package body Sem_Ch8 is

   ------------------------------------
   -- Visibility and Name Resolution --
   ------------------------------------

   --  This package handles name resolution and the collection of possible
   --  interpretations for overloaded names, prior to overload resolution.

   --  Name resolution is the process that establishes a mapping between source
   --  identifiers and the entities they denote at each point in the program.
   --  Each entity is represented by a defining occurrence. Each identifier
   --  that denotes an entity points to the corresponding defining occurrence.
   --  This is the entity of the applied occurrence. Each occurrence holds
   --  an index into the names table, where source identifiers are stored.

   --  Each entry in the names table for an identifier or designator uses the
   --  Info pointer to hold a link to the currently visible entity that has
   --  this name (see subprograms Get_Name_Entity_Id and Set_Name_Entity_Id
   --  in package Sem_Util). The visibility is initialized at the beginning of
   --  semantic processing to make entities in package Standard immediately
   --  visible. The visibility table is used in a more subtle way when
   --  compiling subunits (see below).

   --  Entities that have the same name (i.e. homonyms) are chained. In the
   --  case of overloaded entities, this chain holds all the possible meanings
   --  of a given identifier. The process of overload resolution uses type
   --  information to select from this chain the unique meaning of a given
   --  identifier.

   --  Entities are also chained in their scope, through the Next_Entity link.
   --  As a consequence, the name space is organized as a sparse matrix, where
   --  each row corresponds to a scope, and each column to a source identifier.
   --  Open scopes, that is to say scopes currently being compiled, have their
   --  corresponding rows of entities in order, innermost scope first.

   --  The scopes of packages that are mentioned in context clauses appear in
   --  no particular order, interspersed among open scopes. This is because
   --  in the course of analyzing the context of a compilation, a package
   --  declaration is first an open scope, and subsequently an element of the
   --  context. If subunits or child units are present, a parent unit may
   --  appear under various guises at various times in the compilation.

   --  When the compilation of the innermost scope is complete, the entities
   --  defined therein are no longer visible. If the scope is not a package
   --  declaration, these entities are never visible subsequently, and can be
   --  removed from visibility chains. If the scope is a package declaration,
   --  its visible declarations may still be accessible. Therefore the entities
   --  defined in such a scope are left on the visibility chains, and only
   --  their visibility (immediately visibility or potential use-visibility)
   --  is affected.

   --  The ordering of homonyms on their chain does not necessarily follow
   --  the order of their corresponding scopes on the scope stack. For
   --  example, if package P and the enclosing scope both contain entities
   --  named E, then when compiling the package body the chain for E will
   --  hold the global entity first,  and the local one (corresponding to
   --  the current inner scope) next. As a result, name resolution routines
   --  do not assume any relative ordering of the homonym chains, either
   --  for scope nesting or to order of appearance of context clauses.

   --  When compiling a child unit, entities in the parent scope are always
   --  immediately visible. When compiling the body of a child unit, private
   --  entities in the parent must also be made immediately visible. There
   --  are separate routines to make the visible and private declarations
   --  visible at various times (see package Sem_Ch7).

   --              +--------+         +-----+
   --              | In use |-------->| EU1 |-------------------------->
   --              +--------+         +-----+
   --                                    |                      |
   --      +--------+                 +-----+                +-----+
   --      | Stand. |---------------->| ES1 |--------------->| ES2 |--->
   --      +--------+                 +-----+                +-----+
   --                                    |                      |
   --              +---------+           |                   +-----+
   --              | with'ed |------------------------------>| EW2 |--->
   --              +---------+           |                   +-----+
   --                                    |                      |
   --      +--------+                 +-----+                +-----+
   --      | Scope2 |---------------->| E12 |--------------->| E22 |--->
   --      +--------+                 +-----+                +-----+
   --                                    |                      |
   --      +--------+                 +-----+                +-----+
   --      | Scope1 |---------------->| E11 |--------------->| E12 |--->
   --      +--------+                 +-----+                +-----+
   --          ^                         |                      |
   --          |                         |                      |
   --          |   +---------+           |                      |
   --          |   | with'ed |----------------------------------------->
   --          |   +---------+           |                      |
   --          |                         |                      |
   --      Scope stack                   |                      |
   --      (innermost first)             |                      |
   --                                 +----------------------------+
   --      Names  table =>            | Id1 |     |    |     | Id2 |
   --                                 +----------------------------+

   --  Name resolution must deal with several syntactic forms: simple names,
   --  qualified names, indexed names, and various forms of calls.

   --  Each identifier points to an entry in the names table. The resolution
   --  of a simple name consists in traversing the homonym chain, starting
   --  from the names table. If an entry is immediately visible, it is the one
   --  designated by the identifier. If only potentially use-visible entities
   --  are on the chain, we must verify that they do not hide each other. If
   --  the entity we find is overloadable, we collect all other overloadable
   --  entities on the chain as long as they are not hidden.
   --
   --  To resolve expanded names, we must find the entity at the intersection
   --  of the entity chain for the scope (the prefix) and the homonym chain
   --  for the selector. In general, homonym chains will be much shorter than
   --  entity chains, so it is preferable to start from the names table as
   --  well. If the entity found is overloadable, we must collect all other
   --  interpretations that are defined in the scope denoted by the prefix.

   --  For records, protected types, and tasks, their local entities are
   --  removed from visibility chains on exit from the corresponding scope.
   --  From the outside, these entities are always accessed by selected
   --  notation, and the entity chain for the record type, protected type,
   --  etc. is traversed sequentially in order to find the designated entity.

   --  The discriminants of a type and the operations of a protected type or
   --  task are unchained on  exit from the first view of the type, (such as
   --  a private or incomplete type declaration, or a protected type speci-
   --  fication) and re-chained when compiling the second view.

   --  In the case of operators,  we do not make operators on derived types
   --  explicit. As a result, the notation P."+" may denote either a user-
   --  defined function with name "+", or else an implicit declaration of the
   --  operator "+" in package P. The resolution of expanded names always
   --  tries to resolve an operator name as such an implicitly defined entity,
   --  in addition to looking for explicit declarations.

   --  All forms of names that denote entities (simple names, expanded names,
   --  character literals in some cases) have a Entity attribute, which
   --  identifies the entity denoted by the name.

   ---------------------
   -- The Scope Stack --
   ---------------------

   --  The Scope stack keeps track of the scopes currently been compiled.
   --  Every entity that contains declarations (including records) is placed
   --  on the scope stack while it is being processed, and removed at the end.
   --  Whenever a non-package scope is exited, the entities defined therein
   --  are removed from the visibility table, so that entities in outer scopes
   --  become visible (see previous description). On entry to Sem, the scope
   --  stack only contains the package Standard. As usual, subunits complicate
   --  this picture ever so slightly.

   --  The Rtsfind mechanism can force a call to Semantics while another
   --  compilation is in progress. The unit retrieved by Rtsfind must be
   --  compiled in its own context, and has no access to the visibility of
   --  the unit currently being compiled. The procedures Save_Scope_Stack and
   --  Restore_Scope_Stack make entities in current open scopes invisible
   --  before compiling the retrieved unit, and restore the compilation
   --  environment afterwards.

   ------------------------
   -- Compiling subunits --
   ------------------------

   --  Subunits must be compiled in the environment of the corresponding stub,
   --  that is to say with the same visibility into the parent (and its
   --  context) that is available at the point of the stub declaration, but
   --  with the additional visibility provided by the context clause of the
   --  subunit itself. As a result, compilation of a subunit forces compilation
   --  of the parent (see description in lib-). At the point of the stub
   --  declaration, Analyze is called recursively to compile the proper body of
   --  the subunit, but without reinitializing the names table, nor the scope
   --  stack (i.e. standard is not pushed on the stack). In this fashion the
   --  context of the subunit is added to the context of the parent, and the
   --  subunit is compiled in the correct environment. Note that in the course
   --  of processing the context of a subunit, Standard will appear twice on
   --  the scope stack: once for the parent of the subunit, and once for the
   --  unit in the context clause being compiled. However, the two sets of
   --  entities are not linked by homonym chains, so that the compilation of
   --  any context unit happens in a fresh visibility environment.

   -------------------------------
   -- Processing of USE Clauses --
   -------------------------------

   --  Every defining occurrence has a flag indicating if it is potentially use
   --  visible. Resolution of simple names examines this flag. The processing
   --  of use clauses consists in setting this flag on all visible entities
   --  defined in the corresponding package. On exit from the scope of the use
   --  clause, the corresponding flag must be reset. However, a package may
   --  appear in several nested use clauses (pathological but legal, alas)
   --  which forces us to use a slightly more involved scheme:

   --    a) The defining occurrence for a package holds a flag -In_Use- to
   --    indicate that it is currently in the scope of a use clause. If a
   --    redundant use clause is encountered, then the corresponding occurrence
   --    of the package name is flagged -Redundant_Use-.

   --    b) On exit from a scope, the use clauses in its declarative part are
   --    scanned. The visibility flag is reset in all entities declared in
   --    package named in a use clause, as long as the package is not flagged
   --    as being in a redundant use clause (in which case the outer use
   --    clause is still in effect, and the direct visibility of its entities
   --    must be retained).

   --  Note that entities are not removed from their homonym chains on exit
   --  from the package specification. A subsequent use clause does not need
   --  to rechain the visible entities, but only to establish their direct
   --  visibility.

   -----------------------------------
   -- Handling private declarations --
   -----------------------------------

   --  The principle that each entity has a single defining occurrence clashes
   --  with the presence of two separate definitions for private types: the
   --  first is the private type declaration, and second is the full type
   --  declaration. It is important that all references to the type point to
   --  the same defining occurrence, namely the first one. To enforce the two
   --  separate views of the entity, the corresponding information is swapped
   --  between the two declarations. Outside of the package, the defining
   --  occurrence only contains the private declaration information, while in
   --  the private part and the body of the package the defining occurrence
   --  contains the full declaration. To simplify the swap, the defining
   --  occurrence that currently holds the private declaration points to the
   --  full declaration. During semantic processing the defining occurrence
   --  also points to a list of private dependents, that is to say access types
   --  or composite types whose designated types or component types are
   --  subtypes or derived types of the private type in question. After the
   --  full declaration has been seen, the private dependents are updated to
   --  indicate that they have full definitions.

   ------------------------------------
   -- Handling of Undefined Messages --
   ------------------------------------

   --  In normal mode, only the first use of an undefined identifier generates
   --  a message. The table Urefs is used to record error messages that have
   --  been issued so that second and subsequent ones do not generate further
   --  messages. However, the second reference causes text to be added to the
   --  original undefined message noting "(more references follow)". The
   --  full error list option (-gnatf) forces messages to be generated for
   --  every reference and disconnects the use of this table.

   type Uref_Entry is record
      Node : Node_Id;
      --  Node for identifier for which original message was posted. The
      --  Chars field of this identifier is used to detect later references
      --  to the same identifier.

      Err : Error_Msg_Id;
      --  Records error message Id of original undefined message. Reset to
      --  No_Error_Msg after the second occurrence, where it is used to add
      --  text to the original message as described above.

      Nvis : Boolean;
      --  Set if the message is not visible rather than undefined

      Loc : Source_Ptr;
      --  Records location of error message. Used to make sure that we do
      --  not consider a, b : undefined as two separate instances, which
      --  would otherwise happen, since the parser converts this sequence
      --  to a : undefined; b : undefined.

   end record;

   package Urefs is new Table.Table (
     Table_Component_Type => Uref_Entry,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "Urefs");

   Candidate_Renaming : Entity_Id;
   --  Holds a candidate interpretation that appears in a subprogram renaming
   --  declaration and does not match the given specification, but matches at
   --  least on the first formal. Allows better error message when given
   --  specification omits defaulted parameters, a common error.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Generic_Renaming
     (N : Node_Id;
      K : Entity_Kind);
   --  Common processing for all three kinds of generic renaming declarations.
   --  Enter new name and indicate that it renames the generic unit.

   procedure Analyze_Renamed_Character
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean);
   --  Renamed entity is given by a character literal, which must belong
   --  to the return type of the new entity. Is_Body indicates whether the
   --  declaration is a renaming_as_body. If the original declaration has
   --  already been frozen (because of an intervening body, e.g.) the body of
   --  the function must be built now. The same applies to the following
   --  various renaming procedures.

   procedure Analyze_Renamed_Dereference
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean);
   --  Renamed entity is given by an explicit dereference. Prefix must be a
   --  conformant access_to_subprogram type.

   procedure Analyze_Renamed_Entry
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean);
   --  If the renamed entity in a subprogram renaming is an entry or protected
   --  subprogram, build a body for the new entity whose only statement is a
   --  call to the renamed entity.

   procedure Analyze_Renamed_Family_Member
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean);
   --  Used when the renamed entity is an indexed component. The prefix must
   --  denote an entry family.

   procedure Analyze_Renamed_Primitive_Operation
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean);
   --  If the renamed entity in a subprogram renaming is a primitive operation
   --  or a class-wide operation in prefix form, save the target object,
   --  which must be added to the list of actuals in any subsequent call.
   --  The renaming operation is intrinsic because the compiler must in
   --  fact generate a wrapper for it (6.3.1 (10 1/2)).

   procedure Attribute_Renaming (N : Node_Id);
   --  Analyze renaming of attribute as subprogram. The renaming declaration N
   --  is rewritten as a subprogram body that returns the attribute reference
   --  applied to the formals of the function.

   procedure Set_Entity_Or_Discriminal (N : Node_Id; E : Entity_Id);
   --  Set Entity, with style check if need be. For a discriminant reference,
   --  replace by the corresponding discriminal, i.e. the parameter of the
   --  initialization procedure that corresponds to the discriminant.

   procedure Check_Frozen_Renaming (N : Node_Id; Subp : Entity_Id);
   --  A renaming_as_body may occur after the entity of the original decla-
   --  ration has been frozen. In that case, the body of the new entity must
   --  be built now, because the usual mechanism of building the renamed
   --  body at the point of freezing will not work. Subp is the subprogram
   --  for which N provides the Renaming_As_Body.

   procedure Check_In_Previous_With_Clause (N, Nam : Node_Id);
   --  N is a use_package clause and Nam the package name, or N is a use_type
   --  clause and Nam is the prefix of the type name. In either case, verify
   --  that the package is visible at that point in the context: either it
   --  appears in a previous with_clause, or because it is a fully qualified
   --  name and the root ancestor appears in a previous with_clause.

   procedure Check_Library_Unit_Renaming (N : Node_Id; Old_E : Entity_Id);
   --  Verify that the entity in a renaming declaration that is a library unit
   --  is itself a library unit and not a nested unit or subunit. Also check
   --  that if the renaming is a child unit of a generic parent, then the
   --  renamed unit must also be a child unit of that parent. Finally, verify
   --  that a renamed generic unit is not an implicit child declared within
   --  an instance of the parent.

   procedure Chain_Use_Clause (N : Node_Id);
   --  Chain use clause onto list of uses clauses headed by First_Use_Clause in
   --  the proper scope table entry. This is usually the current scope, but it
   --  will be an inner scope when installing the use clauses of the private
   --  declarations of a parent unit prior to compiling the private part of a
   --  child unit. This chain is traversed when installing/removing use clauses
   --  when compiling a subunit or instantiating a generic body on the fly,
   --  when it is necessary to save and restore full environments.

   function Enclosing_Instance return Entity_Id;
   --  In an instance nested within another one, several semantic checks are
   --  unnecessary because the legality of the nested instance has been checked
   --  in the enclosing generic unit. This applies in particular to legality
   --  checks on actuals for formal subprograms of the inner instance, which
   --  are checked as subprogram renamings, and may be complicated by confusion
   --  in private/full views. This function returns the instance enclosing the
   --  current one if there is such, else it returns Empty.
   --
   --  If the renaming determines the entity for the default of a formal
   --  subprogram nested within another instance, choose the innermost
   --  candidate. This is because if the formal has a box, and we are within
   --  an enclosing instance where some candidate interpretations are local
   --  to this enclosing instance, we know that the default was properly
   --  resolved when analyzing the generic, so we prefer the local
   --  candidates to those that are external. This is not always the case
   --  but is a reasonable heuristic on the use of nested generics. The
   --  proper solution requires a full renaming model.

   function Entity_Of_Unit (U : Node_Id) return Entity_Id;
   --  Return the appropriate entity for determining which unit has a deeper
   --  scope: the defining entity for U, unless U is a package instance, in
   --  which case we retrieve the entity of the instance spec.

   procedure Error_Missing_With_Of_Known_Unit (Pkg : Node_Id);
   --  Display an error message denoting a "with" is missing for a given known
   --  package Pkg with its full path name.

   procedure Find_Expanded_Name (N : Node_Id);
   --  The input is a selected component known to be an expanded name. Verify
   --  legality of selector given the scope denoted by prefix, and change node
   --  N into a expanded name with a properly set Entity field.

   function Find_First_Use (Use_Clause : Node_Id) return Node_Id;
   --  Find the most previous use clause (that is, the first one to appear in
   --  the source) by traversing the previous clause chain that exists in both
   --  N_Use_Package_Clause nodes and N_Use_Type_Clause nodes.

   function Find_Renamed_Entity
     (N         : Node_Id;
      Nam       : Node_Id;
      New_S     : Entity_Id;
      Is_Actual : Boolean := False) return Entity_Id;
   --  Find the renamed entity that corresponds to the given parameter profile
   --  in a subprogram renaming declaration. The renamed entity may be an
   --  operator, a subprogram, an entry, or a protected operation. Is_Actual
   --  indicates that the renaming is the one generated for an actual subpro-
   --  gram in an instance, for which special visibility checks apply.

   function Has_Implicit_Character_Literal (N : Node_Id) return Boolean;
   --  Find a type derived from Character or Wide_Character in the prefix of N.
   --  Used to resolved qualified names whose selector is a character literal.

   function Has_Private_With (E : Entity_Id) return Boolean;
   --  Ada 2005 (AI-262): Determines if the current compilation unit has a
   --  private with on E.

   function Has_Components (Typ : Entity_Id) return Boolean;
   --  Determine if given type has components, i.e. is either a record type or
   --  type or a type that has discriminants.

   function Has_Implicit_Operator (N : Node_Id) return Boolean;
   --  N is an expanded name whose selector is an operator name (e.g. P."+").
   --  Determine if N denotes an operator implicitly declared in prefix P: P's
   --  declarative part contains an implicit declaration of an operator if it
   --  has a declaration of a type to which one of the predefined operators
   --  apply. The existence of this routine is an implementation artifact. A
   --  more straightforward but more space-consuming choice would be to make
   --  all inherited operators explicit in the symbol table.

   procedure Inherit_Renamed_Profile (New_S : Entity_Id; Old_S : Entity_Id);
   --  A subprogram defined by a renaming declaration inherits the parameter
   --  profile of the renamed entity. The subtypes given in the subprogram
   --  specification are discarded and replaced with those of the renamed
   --  subprogram, which are then used to recheck the default values.

   function Most_Descendant_Use_Clause
     (Clause1 : Entity_Id;
      Clause2 : Entity_Id) return Entity_Id;
   --  Determine which use clause parameter is the most descendant in terms of
   --  scope.

   procedure Premature_Usage (N : Node_Id);
   --  Diagnose usage of an entity before it is visible

   function Is_Self_Hidden (E : Entity_Id) return Boolean;
   --  True within a declaration if it is hidden from all visibility by itself
   --  (see RM-8.3(16-18)). This is mostly just "not Is_Not_Self_Hidden", but
   --  we need to check for E_Void in case of errors.

   procedure Use_One_Package
     (N         : Node_Id;
      Pack_Name : Entity_Id := Empty;
      Force     : Boolean   := False);
   --  Make visible entities declared in package P potentially use-visible
   --  in the current context. Also used in the analysis of subunits, when
   --  re-installing use clauses of parent units. N is the use_clause that
   --  names P (and possibly other packages).

   procedure Use_One_Type
     (Id        : Node_Id;
      Installed : Boolean := False;
      Force     : Boolean := False);
   --  Id is the subtype mark from a use_type_clause. This procedure makes
   --  the primitive operators of the type potentially use-visible. The
   --  boolean flag Installed indicates that the clause is being reinstalled
   --  after previous analysis, and primitive operations are already chained
   --  on the Used_Operations list of the clause.

   procedure Write_Info;
   --  Write debugging information on entities declared in current scope

   --------------------------------
   -- Analyze_Exception_Renaming --
   --------------------------------

   --  The language only allows a single identifier, but the tree holds an
   --  identifier list. The parser has already issued an error message if
   --  there is more than one element in the list.

   procedure Analyze_Exception_Renaming (N : Node_Id) is
      Id  : constant Entity_Id := Defining_Entity (N);
      Nam : constant Node_Id   := Name (N);

   begin
      Enter_Name (Id);
      Analyze (Nam);

      Mutate_Ekind   (Id, E_Exception);
      Set_Etype   (Id, Standard_Exception_Type);
      Set_Is_Pure (Id, Is_Pure (Current_Scope));

      if Is_Entity_Name (Nam)
        and then Present (Entity (Nam))
        and then Ekind (Entity (Nam)) = E_Exception
      then
         if Present (Renamed_Entity (Entity (Nam))) then
            Set_Renamed_Entity (Id, Renamed_Entity (Entity (Nam)));
         else
            Set_Renamed_Entity (Id, Entity (Nam));
         end if;

         --  The exception renaming declaration may become Ghost if it renames
         --  a Ghost entity.

         Mark_Ghost_Renaming (N, Entity (Nam));
      else
         Error_Msg_N ("invalid exception name in renaming", Nam);
      end if;

      --  Implementation-defined aspect specifications can appear in a renaming
      --  declaration, but not language-defined ones. The call to procedure
      --  Analyze_Aspect_Specifications will take care of this error check.

      Analyze_Aspect_Specifications (N, Id);
   end Analyze_Exception_Renaming;

   ---------------------------
   -- Analyze_Expanded_Name --
   ---------------------------

   procedure Analyze_Expanded_Name (N : Node_Id) is
   begin
      --  If the entity pointer is already set, this is an internal node, or a
      --  node that is analyzed more than once, after a tree modification. In
      --  such a case there is no resolution to perform, just set the type. In
      --  either case, start by analyzing the prefix.

      Analyze (Prefix (N));

      if Present (Entity (N)) then
         if Is_Type (Entity (N)) then
            Set_Etype (N, Entity (N));
         else
            Set_Etype (N, Etype (Entity (N)));
         end if;

      else
         Find_Expanded_Name (N);
      end if;

      --  In either case, propagate dimension of entity to expanded name

      Analyze_Dimension (N);
   end Analyze_Expanded_Name;

   ---------------------------------------
   -- Analyze_Generic_Function_Renaming --
   ---------------------------------------

   procedure Analyze_Generic_Function_Renaming  (N : Node_Id) is
   begin
      Analyze_Generic_Renaming (N, E_Generic_Function);
   end Analyze_Generic_Function_Renaming;

   --------------------------------------
   -- Analyze_Generic_Package_Renaming --
   --------------------------------------

   procedure Analyze_Generic_Package_Renaming   (N : Node_Id) is
   begin
      --  Test for the Text_IO special unit case here, since we may be renaming
      --  one of the subpackages of Text_IO, then join common routine.

      Check_Text_IO_Special_Unit (Name (N));

      Analyze_Generic_Renaming (N, E_Generic_Package);
   end Analyze_Generic_Package_Renaming;

   ----------------------------------------
   -- Analyze_Generic_Procedure_Renaming --
   ----------------------------------------

   procedure Analyze_Generic_Procedure_Renaming (N : Node_Id) is
   begin
      Analyze_Generic_Renaming (N, E_Generic_Procedure);
   end Analyze_Generic_Procedure_Renaming;

   ------------------------------
   -- Analyze_Generic_Renaming --
   ------------------------------

   procedure Analyze_Generic_Renaming
     (N : Node_Id;
      K : Entity_Kind)
   is
      New_P : constant Entity_Id := Defining_Entity (N);
      Inst  : Boolean := False;
      Old_P : Entity_Id;

   begin
      if Name (N) = Error then
         return;
      end if;

      Generate_Definition (New_P);

      if Current_Scope /= Standard_Standard then
         Set_Is_Pure (New_P, Is_Pure (Current_Scope));
      end if;

      if Nkind (Name (N)) = N_Selected_Component then
         Check_Generic_Child_Unit (Name (N), Inst);
      else
         Analyze (Name (N));
      end if;

      if not Is_Entity_Name (Name (N)) then
         Error_Msg_N ("expect entity name in renaming declaration", Name (N));
         Old_P := Any_Id;
      else
         Old_P := Entity (Name (N));
      end if;

      Enter_Name (New_P);
      Mutate_Ekind (New_P, K);

      if Etype (Old_P) = Any_Type then
         null;

      elsif Ekind (Old_P) /= K then
         Error_Msg_N ("invalid generic unit name", Name (N));

      else
         if Present (Renamed_Entity (Old_P)) then
            Set_Renamed_Entity (New_P, Renamed_Entity (Old_P));
         else
            Set_Renamed_Entity (New_P, Old_P);
         end if;

         --  The generic renaming declaration may become Ghost if it renames a
         --  Ghost entity.

         Mark_Ghost_Renaming (N, Old_P);

         Set_Is_Pure          (New_P, Is_Pure          (Old_P));
         Set_Is_Preelaborated (New_P, Is_Preelaborated (Old_P));

         Set_Etype (New_P, Etype (Old_P));
         Set_Has_Completion (New_P);

         if In_Open_Scopes (Old_P) then
            Error_Msg_N ("within its scope, generic denotes its instance", N);
         end if;

         --  For subprograms, propagate the Intrinsic flag, to allow, e.g.
         --  renamings and subsequent instantiations of Unchecked_Conversion.

         if Is_Generic_Subprogram (Old_P) then
            Set_Is_Intrinsic_Subprogram
              (New_P, Is_Intrinsic_Subprogram (Old_P));
         end if;

         Check_Library_Unit_Renaming (N, Old_P);
      end if;

      --  Implementation-defined aspect specifications can appear in a renaming
      --  declaration, but not language-defined ones. The call to procedure
      --  Analyze_Aspect_Specifications will take care of this error check.

      Analyze_Aspect_Specifications (N, New_P);
   end Analyze_Generic_Renaming;

   -----------------------------
   -- Analyze_Object_Renaming --
   -----------------------------

   procedure Analyze_Object_Renaming (N : Node_Id) is
      Id            : constant Entity_Id  := Defining_Identifier (N);
      Loc           : constant Source_Ptr := Sloc (N);
      Nam           : constant Node_Id    := Name (N);
      Is_Object_Ref : Boolean;
      Dec           : Node_Id;
      T             : Entity_Id;
      T2            : Entity_Id;
      Q             : Node_Id;

      procedure Check_Constrained_Object;
      --  If the nominal type is unconstrained but the renamed object is
      --  constrained, as can happen with renaming an explicit dereference or
      --  a function return, build a constrained subtype from the object. If
      --  the renaming is for a formal in an accept statement, the analysis
      --  has already established its actual subtype. This is only relevant
      --  if the renamed object is an explicit dereference.

      function Get_Object_Name (Nod : Node_Id) return Node_Id;
      --  Obtain the name of the object from node Nod which is being renamed by
      --  the object renaming declaration N.

      function Find_Raise_Node (N : Node_Id) return Traverse_Result;
      --  Process one node in search for N_Raise_xxx_Error nodes.
      --  Return Abandon if found, OK otherwise.

      ---------------------
      -- Find_Raise_Node --
      ---------------------

      function Find_Raise_Node (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) in N_Raise_xxx_Error then
            return Abandon;
         else
            return OK;
         end if;
      end Find_Raise_Node;

      ------------------------
      -- No_Raise_xxx_Error --
      ------------------------

      function No_Raise_xxx_Error is new Traverse_Func (Find_Raise_Node);
      --  Traverse tree to look for a N_Raise_xxx_Error node and returns
      --  Abandon if so and OK if none found.

      ------------------------------
      -- Check_Constrained_Object --
      ------------------------------

      procedure Check_Constrained_Object is
         Typ         : constant Entity_Id := Etype (Nam);
         Subt        : Entity_Id;
         Loop_Scheme : Node_Id;

      begin
         if Nkind (Nam) in N_Function_Call | N_Explicit_Dereference
           and then Is_Composite_Type (Typ)
           and then not Is_Constrained (Typ)
           and then not Has_Unknown_Discriminants (Typ)
           and then Expander_Active
         then
            --  If Actual_Subtype is already set, nothing to do

            if Ekind (Id) in E_Variable | E_Constant
              and then Present (Actual_Subtype (Id))
            then
               null;

            --  A renaming of an unchecked union has no actual subtype

            elsif Is_Unchecked_Union (Typ) then
               null;

            --  If a record is limited its size is invariant. This is the case
            --  in particular with record types with an access discriminant
            --  that are used in iterators. This is an optimization, but it
            --  also prevents typing anomalies when the prefix is further
            --  expanded.

            --  Note that we cannot just use the Is_Limited_Record flag because
            --  it does not apply to records with limited components, for which
            --  this syntactic flag is not set, but whose size is also fixed.

            --  Note also that we need to build the constrained subtype for an
            --  array in order to make the bounds explicit in most cases, but
            --  not if the object comes from an extended return statement, as
            --  this would create dangling references to them later on.

            elsif Is_Limited_Type (Typ)
              and then (not Is_Array_Type (Typ) or else Is_Return_Object (Id))
            then
               null;

            else
               Subt := Make_Temporary (Loc, 'T');
               Remove_Side_Effects (Nam);
               Insert_Action (N,
                 Make_Subtype_Declaration (Loc,
                   Defining_Identifier => Subt,
                   Subtype_Indication  =>
                     Make_Subtype_From_Expr (Nam, Typ)));

               declare
                  New_Subtype_Mark : constant Node_Id :=
                    New_Occurrence_Of (Subt, Loc);
               begin
                  if Present (Subtype_Mark (N)) then
                     Rewrite (Subtype_Mark (N), New_Subtype_Mark);
                  else
                     --  An Ada2022 renaming with no subtype mark
                     Set_Subtype_Mark (N, New_Subtype_Mark);
                  end if;
               end;

               Set_Etype (Nam, Subt);

               --  Suppress discriminant checks on this subtype if the original
               --  type has defaulted discriminants and Id is a "for of" loop
               --  iterator.

               if Has_Defaulted_Discriminants (Typ)
                 and then Nkind (Original_Node (Parent (N))) = N_Loop_Statement
               then
                  Loop_Scheme := Iteration_Scheme (Original_Node (Parent (N)));

                  if Present (Loop_Scheme)
                    and then Present (Iterator_Specification (Loop_Scheme))
                    and then
                      Defining_Identifier
                        (Iterator_Specification (Loop_Scheme)) = Id
                  then
                     Set_Checks_May_Be_Suppressed (Subt);
                     Push_Local_Suppress_Stack_Entry
                       (Entity   => Subt,
                        Check    => Discriminant_Check,
                        Suppress => True);
                  end if;
               end if;

               --  Freeze subtype at once, to prevent order of elaboration
               --  issues in the backend. The renamed object exists, so its
               --  type is already frozen in any case.

               Freeze_Before (N, Subt);
            end if;
         end if;
      end Check_Constrained_Object;

      ---------------------
      -- Get_Object_Name --
      ---------------------

      function Get_Object_Name (Nod : Node_Id) return Node_Id is
         Obj_Nam : Node_Id;

      begin
         Obj_Nam := Nod;
         while Present (Obj_Nam) loop
            case Nkind (Obj_Nam) is
               when N_Attribute_Reference
                  | N_Explicit_Dereference
                  | N_Indexed_Component
                  | N_Slice
               =>
                  Obj_Nam := Prefix (Obj_Nam);

               when N_Selected_Component =>
                  Obj_Nam := Selector_Name (Obj_Nam);

               when N_Qualified_Expression | N_Type_Conversion =>
                  Obj_Nam := Expression (Obj_Nam);

               when others =>
                  exit;
            end case;
         end loop;

         return Obj_Nam;
      end Get_Object_Name;

   --  Start of processing for Analyze_Object_Renaming

   begin
      if Nam = Error then
         return;
      end if;

      Set_Is_Pure (Id, Is_Pure (Current_Scope));
      Enter_Name (Id);

      --  The renaming of a component that depends on a discriminant requires
      --  an actual subtype, because in subsequent use of the object Gigi will
      --  be unable to locate the actual bounds. This explicit step is required
      --  when the renaming is generated in removing side effects of an
      --  already-analyzed expression.

      if Nkind (Nam) = N_Selected_Component and then Analyzed (Nam) then

         --  The object renaming declaration may become Ghost if it renames a
         --  Ghost entity.

         if Is_Entity_Name (Nam) then
            Mark_Ghost_Renaming (N, Entity (Nam));
         end if;

         T   := Etype (Nam);
         Dec := Build_Actual_Subtype_Of_Component (Etype (Nam), Nam);

         if Present (Dec) then
            Insert_Action (N, Dec);
            T := Defining_Identifier (Dec);
            Set_Etype (Nam, T);
         end if;
      elsif Present (Subtype_Mark (N))
        or else No (Access_Definition (N))
      then
         if Present (Subtype_Mark (N)) then
            Find_Type (Subtype_Mark (N));
            T := Entity (Subtype_Mark (N));
            Analyze (Nam);

         --  AI12-0275: Case of object renaming without a subtype_mark

         else
            Analyze (Nam);

            --  Normal case of no overloading in object name

            if not Is_Overloaded (Nam) then

               --  Catch error cases (such as attempting to rename a procedure
               --  or package) using the shorthand form.

               if No (Etype (Nam))
                 or else Etype (Nam) = Standard_Void_Type
               then
                  Error_Msg_N
                    ("object name or value expected in renaming", Nam);

                  Mutate_Ekind (Id, E_Variable);
                  Set_Etype (Id, Any_Type);

                  return;

               else
                  T := Etype (Nam);
               end if;

            --  Case of overloaded name, which will be illegal if there's more
            --  than one acceptable interpretation (such as overloaded function
            --  calls).

            else
               declare
                  I    : Interp_Index;
                  I1   : Interp_Index;
                  It   : Interp;
                  It1  : Interp;
                  Nam1 : Entity_Id;

               begin
                  --  More than one candidate interpretation is available

                  --  Remove procedure calls, which syntactically cannot appear
                  --  in this context, but which cannot be removed by type
                  --  checking, because the context does not impose a type.

                  Get_First_Interp (Nam, I, It);
                  while Present (It.Typ) loop
                     if It.Typ = Standard_Void_Type then
                        Remove_Interp (I);
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;

                  Get_First_Interp (Nam, I, It);
                  I1  := I;
                  It1 := It;

                  --  If there's no type present, we have an error case (such
                  --  as overloaded procedures named in the object renaming).

                  if No (It.Typ) then
                     Error_Msg_N
                       ("object name or value expected in renaming", Nam);

                     Mutate_Ekind (Id, E_Variable);
                     Set_Etype (Id, Any_Type);

                     return;
                  end if;

                  Get_Next_Interp (I, It);

                  if Present (It.Typ) then
                     Nam1  := It1.Nam;
                     It1 := Disambiguate (Nam, I1, I, Any_Type);

                     if It1 = No_Interp then
                        Error_Msg_N ("ambiguous name in object renaming", Nam);

                        Error_Msg_Sloc := Sloc (It.Nam);
                        Error_Msg_N ("\\possible interpretation#!", Nam);

                        Error_Msg_Sloc := Sloc (Nam1);
                        Error_Msg_N ("\\possible interpretation#!", Nam);

                        return;
                     end if;
                  end if;

                  Set_Etype (Nam, It1.Typ);
                  T := It1.Typ;
               end;
            end if;

            if Etype (Nam) = Standard_Exception_Type then
               Error_Msg_N
                 ("exception requires a subtype mark in renaming", Nam);
               return;
            end if;
         end if;

         --  The object renaming declaration may become Ghost if it renames a
         --  Ghost entity.

         if Is_Entity_Name (Nam) then
            Mark_Ghost_Renaming (N, Entity (Nam));
         end if;

         --  Check against AI12-0401 here before Resolve may rewrite Nam and
         --  potentially generate spurious warnings.

         --   In the case where the object_name is a qualified_expression with
         --   a nominal subtype T and whose expression is a name that denotes
         --   an object Q:
         --    * if T is an elementary subtype, then:
         --      * Q shall be a constant other than a dereference of an access
         --        type; or
         --      * the nominal subtype of Q shall be statically compatible with
         --        T; or
         --      * T shall statically match the base subtype of its type if
         --        scalar, or the first subtype of its type if an access type.
         --    * if T is a composite subtype, then Q shall be known to be
         --      constrained or T shall statically match the first subtype of
         --      its type.

         if Nkind (Nam) = N_Qualified_Expression
           and then Is_Object_Reference (Expression (Nam))
         then
            Q := Expression (Nam);

            if (Is_Elementary_Type (T)
                  and then
                not ((not Is_Variable (Q)
                       and then Nkind (Q) /= N_Explicit_Dereference)
                      or else Subtypes_Statically_Compatible (Etype (Q), T)
                      or else (Is_Scalar_Type (T)
                                and then Subtypes_Statically_Match
                                           (T, Base_Type (T)))
                      or else (Is_Access_Type (T)
                                and then Subtypes_Statically_Match
                                           (T, First_Subtype (T)))))
              or else (Is_Composite_Type (T)
                         and then

                       --  If Q is an aggregate, Is_Constrained may not be set
                       --  yet and its type may not be resolved yet.
                       --  This doesn't quite correspond to the complex notion
                       --  of "known to be constrained" but this is good enough
                       --  for a rule which is in any case too complex.

                       not (Is_Constrained (Etype (Q))
                             or else Nkind (Q) = N_Aggregate
                             or else Subtypes_Statically_Match
                                       (T, First_Subtype (T))))
            then
               Error_Msg_N
                 ("subtype of renamed qualified expression does not " &
                  "statically match", N);
               return;
            end if;
         end if;

         Resolve (Nam, T);

         --  If the renamed object is a function call of a limited type,
         --  the expansion of the renaming is complicated by the presence
         --  of various temporaries and subtypes that capture constraints
         --  of the renamed object. Rewrite node as an object declaration,
         --  whose expansion is simpler. Given that the object is limited
         --  there is no copy involved and no performance hit.

         if Nkind (Nam) = N_Function_Call
           and then Is_Inherently_Limited_Type (Etype (Nam))
           and then not Is_Constrained (Etype (Nam))
           and then Comes_From_Source (N)
         then
            Set_Etype (Id, T);
            Mutate_Ekind (Id, E_Constant);
            Rewrite (N,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Id,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (Etype (Nam), Loc),
                Expression          => Relocate_Node (Nam)));
            return;
         end if;

         --  Ada 2012 (AI05-149): Reject renaming of an anonymous access object
         --  when renaming declaration has a named access type. The Ada 2012
         --  coverage rules allow an anonymous access type in the context of
         --  an expected named general access type, but the renaming rules
         --  require the types to be the same. (An exception is when the type
         --  of the renaming is also an anonymous access type, which can only
         --  happen due to a renaming created by the expander.)

         if Nkind (Nam) = N_Type_Conversion
           and then not Comes_From_Source (Nam)
           and then Is_Anonymous_Access_Type (Etype (Expression (Nam)))
           and then not Is_Anonymous_Access_Type (T)
         then
            Error_Msg_NE
              ("cannot rename anonymous access object "
                & "as a named access type", Expression (Nam), T);
         end if;

         --  Check that a class-wide object is not being renamed as an object
         --  of a specific type. The test for access types is needed to exclude
         --  cases where the renamed object is a dynamically tagged access
         --  result, such as occurs in certain expansions.

         if Is_Tagged_Type (T) then
            Check_Dynamically_Tagged_Expression
              (Expr        => Nam,
               Typ         => T,
               Related_Nod => N);
         end if;

      --  Ada 2005 (AI-230/AI-254): Access renaming

      else pragma Assert (Present (Access_Definition (N)));
         T :=
           Access_Definition
             (Related_Nod => N,
              N           => Access_Definition (N));

         Analyze (Nam);

         --  The object renaming declaration may become Ghost if it renames a
         --  Ghost entity.

         if Is_Entity_Name (Nam) then
            Mark_Ghost_Renaming (N, Entity (Nam));
         end if;

         --  Ada 2005 AI05-105: if the declaration has an anonymous access
         --  type, the renamed object must also have an anonymous type, and
         --  this is a name resolution rule. This was implicit in the last part
         --  of the first sentence in 8.5.1(3/2), and is made explicit by this
         --  recent AI.

         if not Is_Overloaded (Nam) then
            if Ekind (Etype (Nam)) /= Ekind (T) then
               Error_Msg_N
                 ("expect anonymous access type in object renaming", N);
            end if;

         else
            declare
               I    : Interp_Index;
               It   : Interp;
               Typ  : Entity_Id := Empty;
               Seen : Boolean   := False;

            begin
               Get_First_Interp (Nam, I, It);
               while Present (It.Typ) loop

                  --  Renaming is ambiguous if more than one candidate
                  --  interpretation is type-conformant with the context.

                  if Ekind (It.Typ) = Ekind (T) then
                     if Ekind (T) = E_Anonymous_Access_Subprogram_Type
                       and then
                         Type_Conformant
                           (Designated_Type (T), Designated_Type (It.Typ))
                     then
                        if not Seen then
                           Seen := True;
                        else
                           Error_Msg_N
                             ("ambiguous expression in renaming", Nam);
                        end if;

                     elsif Ekind (T) = E_Anonymous_Access_Type
                       and then
                         Covers (Designated_Type (T), Designated_Type (It.Typ))
                     then
                        if not Seen then
                           Seen := True;
                        else
                           Error_Msg_N
                             ("ambiguous expression in renaming", Nam);
                        end if;
                     end if;

                     if Covers (T, It.Typ) then
                        Typ := It.Typ;
                        Set_Etype (Nam, Typ);
                        Set_Is_Overloaded (Nam, False);
                     end if;
                  end if;

                  Get_Next_Interp (I, It);
               end loop;
            end;
         end if;

         Resolve (Nam, T);

         --  Do not perform the legality checks below when the resolution of
         --  the renaming name failed because the associated type is Any_Type.

         if Etype (Nam) = Any_Type then
            null;

         --  Ada 2005 (AI-231): In the case where the type is defined by an
         --  access_definition, the renamed entity shall be of an access-to-
         --  constant type if and only if the access_definition defines an
         --  access-to-constant type. ARM 8.5.1(4)

         elsif Constant_Present (Access_Definition (N))
           and then not Is_Access_Constant (Etype (Nam))
         then
            Error_Msg_N
              ("(Ada 2005): the renamed object is not access-to-constant "
               & "(RM 8.5.1(6))", N);

         elsif not Constant_Present (Access_Definition (N))
           and then Is_Access_Constant (Etype (Nam))
         then
            Error_Msg_N
              ("(Ada 2005): the renamed object is not access-to-variable "
               & "(RM 8.5.1(6))", N);
         end if;

         if Is_Access_Subprogram_Type (Etype (Nam)) then
            Check_Subtype_Conformant
              (Designated_Type (T), Designated_Type (Etype (Nam)));

         elsif not Subtypes_Statically_Match
                     (Designated_Type (T),
                      Available_View (Designated_Type (Etype (Nam))))
         then
            Error_Msg_N
              ("subtype of renamed object does not statically match", N);
         end if;
      end if;

      --  Special processing for renaming function return object. Some errors
      --  and warnings are produced only for calls that come from source.

      if Nkind (Nam) = N_Function_Call then
         case Ada_Version is

            --  Usage is illegal in Ada 83, but renamings are also introduced
            --  during expansion, and error does not apply to those.

            when Ada_83 =>
               if Comes_From_Source (N) then
                  Error_Msg_N
                    ("(Ada 83) cannot rename function return object", Nam);
               end if;

            --  In Ada 95, warn for odd case of renaming parameterless function
            --  call if this is not a limited type (where this is useful).

            when others =>
               if Warn_On_Object_Renames_Function
                 and then No (Parameter_Associations (Nam))
                 and then not Is_Limited_Type (Etype (Nam))
                 and then Comes_From_Source (Nam)
               then
                  Error_Msg_N
                    ("renaming function result object is suspicious?.r?", Nam);
                  Error_Msg_NE
                    ("\function & will be called only once?.r?", Nam,
                     Entity (Name (Nam)));
                  Error_Msg_N -- CODEFIX
                    ("\suggest using an initialized constant object "
                     & "instead?.r?", Nam);
               end if;
         end case;
      end if;

      Check_Constrained_Object;

      --  An object renaming requires an exact match of the type. Class-wide
      --  matching is not allowed.

      if Is_Class_Wide_Type (T)
        and then Base_Type (Etype (Nam)) /= Base_Type (T)
      then
         Wrong_Type (Nam, T);
      end if;

      --  We must search for an actual subtype here so that the bounds of
      --  objects of unconstrained types don't get dropped on the floor - such
      --  as with renamings of formal parameters.

      T2 := Get_Actual_Subtype_If_Available (Nam);

      --  Ada 2005 (AI-326): Handle wrong use of incomplete type

      if Nkind (Nam) = N_Explicit_Dereference
        and then Ekind (Etype (T2)) = E_Incomplete_Type
      then
         Error_Msg_NE ("invalid use of incomplete type&", Id, T2);
         return;

      elsif Ekind (Etype (T)) = E_Incomplete_Type then
         Error_Msg_NE ("invalid use of incomplete type&", Id, T);
         return;
      end if;

      if Ada_Version >= Ada_2005 and then Nkind (Nam) in N_Has_Entity then
         declare
            Nam_Ent  : constant Entity_Id := Entity (Get_Object_Name (Nam));
            Nam_Decl : constant Node_Id   := Declaration_Node (Nam_Ent);

         begin
            if Has_Null_Exclusion (N)
              and then not Has_Null_Exclusion (Nam_Decl)
            then
               --  Ada 2005 (AI-423): If the object name denotes a generic
               --  formal object of a generic unit G, and the object renaming
               --  declaration occurs within the body of G or within the body
               --  of a generic unit declared within the declarative region
               --  of G, then the declaration of the formal object of G must
               --  have a null exclusion or a null-excluding subtype.

               if Is_Formal_Object (Nam_Ent)
                 and then In_Generic_Scope (Id)
               then
                  if not Can_Never_Be_Null (Etype (Nam_Ent)) then
                     Error_Msg_N
                       ("object does not exclude `NULL` "
                        & "(RM 8.5.1(4.6/2))", N);

                  elsif In_Package_Body (Scope (Id)) then
                     Error_Msg_N
                       ("formal object does not have a null exclusion"
                        & "(RM 8.5.1(4.6/2))", N);
                  end if;

               --  Ada 2005 (AI-423): Otherwise, the subtype of the object name
               --  shall exclude null.

               elsif not Can_Never_Be_Null (Etype (Nam_Ent)) then
                  Error_Msg_N
                    ("object does not exclude `NULL` "
                     & "(RM 8.5.1(4.6/2))", N);

               --  An instance is illegal if it contains a renaming that
               --  excludes null, and the actual does not. The renaming
               --  declaration has already indicated that the declaration
               --  of the renamed actual in the instance will raise
               --  constraint_error.

               elsif Nkind (Nam_Decl) = N_Object_Declaration
                 and then In_Instance
                 and then
                   Present (Corresponding_Generic_Association (Nam_Decl))
                 and then Nkind (Expression (Nam_Decl)) =
                                            N_Raise_Constraint_Error
               then
                  Error_Msg_N
                    ("actual does not exclude `NULL` (RM 8.5.1(4.6/2))", N);

               --  Finally, if there is a null exclusion, the subtype mark
               --  must not be null-excluding.

               elsif No (Access_Definition (N))
                 and then Can_Never_Be_Null (T)
               then
                  Error_Msg_NE
                    ("`NOT NULL` not allowed (& already excludes null)",
                      N, T);

               end if;

            elsif Can_Never_Be_Null (T)
              and then not Can_Never_Be_Null (Etype (Nam_Ent))
            then
               Error_Msg_N
                 ("object does not exclude `NULL` (RM 8.5.1(4.6/2))", N);

            elsif Has_Null_Exclusion (N)
              and then No (Access_Definition (N))
              and then Can_Never_Be_Null (T)
            then
               Error_Msg_NE
                 ("`NOT NULL` not allowed (& already excludes null)", N, T);
            end if;
         end;
      end if;

      --  Set the Ekind of the entity, unless it has been set already, as is
      --  the case for the iteration object over a container with no variable
      --  indexing. In that case it's been marked as a constant, and we do not
      --  want to change it to a variable.

      if Ekind (Id) /= E_Constant then
         Mutate_Ekind (Id, E_Variable);
      end if;

      Reinit_Object_Size_Align (Id);

      --  If N comes from source then check that the original node is an
      --  object reference since there may have been several rewritting and
      --  folding. Do not do this for N_Function_Call or N_Explicit_Dereference
      --  which might correspond to rewrites of e.g. N_Selected_Component
      --  (for example Object.Method rewriting).
      --  If N does not come from source then assume the tree is properly
      --  formed and accept any object reference. In such cases we do support
      --  more cases of renamings anyway, so the actual check on which renaming
      --  is valid is better left to the code generator as a last sanity
      --  check.

      if Comes_From_Source (N) then
         if Nkind (Nam) in N_Function_Call | N_Explicit_Dereference then
            Is_Object_Ref := Is_Object_Reference (Nam);
         else
            Is_Object_Ref := Is_Object_Reference (Original_Node (Nam));
         end if;
      else
         Is_Object_Ref := True;
      end if;

      if T = Any_Type or else Etype (Nam) = Any_Type then
         return;

      --  Verify that the renamed entity is an object or function call

      elsif Is_Object_Ref then
         if Comes_From_Source (N) then
            if Is_Dependent_Component_Of_Mutable_Object (Nam) then
               Error_Msg_N
                 ("illegal renaming of discriminant-dependent component", Nam);
            elsif Depends_On_Mutably_Tagged_Ext_Comp (Nam) then
               Error_Msg_N
                 ("illegal renaming of mutably tagged dependent component",
                  Nam);
            end if;

            --  If the renaming comes from source and the renamed object is a
            --  dereference, then mark the prefix as needing debug information,
            --  since it might have been rewritten hence internally generated
            --  and Debug_Renaming_Declaration will link the renaming to it.

            if Nkind (Nam) = N_Explicit_Dereference
              and then Is_Entity_Name (Prefix (Nam))
            then
               Set_Debug_Info_Needed (Entity (Prefix (Nam)));
            end if;
         end if;

      --  Weird but legal, equivalent to renaming a function call. Illegal
      --  if the literal is the result of constant-folding an attribute
      --  reference that is not a function.

      elsif Is_Entity_Name (Nam)
        and then Ekind (Entity (Nam)) = E_Enumeration_Literal
        and then Nkind (Original_Node (Nam)) /= N_Attribute_Reference
      then
         null;

      --  A named number can only be renamed without a subtype mark

      elsif Nkind (Nam) in N_Real_Literal | N_Integer_Literal
        and then Present (Subtype_Mark (N))
        and then Present (Original_Entity (Nam))
      then
         Error_Msg_N ("incompatible types in renaming", Nam);

      --  AI12-0383: Names that denote values can be renamed.
      --  Ignore (accept) N_Raise_xxx_Error nodes in this context.

      elsif No_Raise_xxx_Error (Nam) = OK then
         Error_Msg_Ada_2022_Feature ("value in renaming", Sloc (Nam));
      end if;

      Set_Etype (Id, T2);

      if not Is_Variable (Nam) then
         Mutate_Ekind            (Id, E_Constant);
         Set_Never_Set_In_Source (Id, True);
         Set_Is_True_Constant    (Id, True);
      end if;

      --  The entity of the renaming declaration needs to reflect whether the
      --  renamed object is atomic, independent, volatile or VFA. These flags
      --  are set on the renamed object in the RM legality sense.

      Set_Is_Atomic (Id, Is_Atomic_Object (Nam));
      Set_Is_Independent (Id, Is_Independent_Object (Nam));
      Set_Is_Volatile (Id, Is_Volatile_Object_Ref (Nam));
      Set_Is_Volatile_Full_Access
        (Id, Is_Volatile_Full_Access_Object_Ref (Nam));

      --  Treat as volatile if we just set the Volatile flag

      if Is_Volatile (Id)

        --  Or if we are renaming an entity which was marked this way

        --  Are there more cases, e.g. X(J) where X is Treat_As_Volatile ???

        or else (Is_Entity_Name (Nam)
                  and then Treat_As_Volatile (Entity (Nam)))
      then
         Set_Treat_As_Volatile (Id, True);
      end if;

      --  Now make the link to the renamed object

      Set_Renamed_Object (Id, Nam);

      --  Implementation-defined aspect specifications can appear in a renaming
      --  declaration, but not language-defined ones. The call to procedure
      --  Analyze_Aspect_Specifications will take care of this error check.

      Analyze_Aspect_Specifications (N, Id);

      --  Deal with dimensions

      Analyze_Dimension (N);
   end Analyze_Object_Renaming;

   ------------------------------
   -- Analyze_Package_Renaming --
   ------------------------------

   procedure Analyze_Package_Renaming (N : Node_Id) is
      New_P : constant Entity_Id := Defining_Entity (N);
      Old_P : Entity_Id;
      Spec  : Node_Id;

   begin
      if Name (N) = Error then
         return;
      end if;

      --  Check for Text_IO special units (we may be renaming a Text_IO child),
      --  but make sure not to catch renamings generated for package instances
      --  that have nothing to do with them but are nevertheless homonyms.

      if Is_Entity_Name (Name (N))
        and then Present (Entity (Name (N)))
        and then Is_Generic_Instance (Entity (Name (N)))
      then
         null;
      else
         Check_Text_IO_Special_Unit (Name (N));
      end if;

      if Current_Scope /= Standard_Standard then
         Set_Is_Pure (New_P, Is_Pure (Current_Scope));
      end if;

      Enter_Name (New_P);
      Analyze (Name (N));

      if Is_Entity_Name (Name (N)) then
         Old_P := Entity (Name (N));
      else
         Old_P := Any_Id;
      end if;

      if Etype (Old_P) = Any_Type then
         Error_Msg_N ("expect package name in renaming", Name (N));

      elsif Ekind (Old_P) /= E_Package
        and then not (Ekind (Old_P) = E_Generic_Package
                       and then In_Open_Scopes (Old_P))
      then
         if Ekind (Old_P) = E_Generic_Package then
            Error_Msg_N
               ("generic package cannot be renamed as a package", Name (N));
         else
            Error_Msg_Sloc := Sloc (Old_P);
            Error_Msg_NE
              ("expect package name in renaming, found& declared#",
               Name (N), Old_P);
         end if;

         --  Set basic attributes to minimize cascaded errors

         Mutate_Ekind (New_P, E_Package);
         Set_Etype (New_P, Standard_Void_Type);

      elsif Present (Renamed_Entity (Old_P))
        and then Renames_Limited_View (Old_P)
        and then not
          Unit_Is_Visible (Cunit (Get_Source_Unit (Renamed_Entity (Old_P))))
      then
         Error_Msg_NE
           ("renaming of limited view of package & not usable in this context"
            & " (RM 8.5.3(3.1/2))", Name (N), Renamed_Entity (Old_P));

         --  Set basic attributes to minimize cascaded errors

         Mutate_Ekind (New_P, E_Package);
         Set_Etype (New_P, Standard_Void_Type);

      --  Here for OK package renaming

      else
         --  Entities in the old package are accessible through the renaming
         --  entity. The simplest implementation is to have both packages share
         --  the entity list.

         Mutate_Ekind (New_P, E_Package);
         Set_Etype (New_P, Standard_Void_Type);

         if Present (Renamed_Entity (Old_P)) then
            Set_Renamed_Entity (New_P, Renamed_Entity (Old_P));
            Set_Renames_Limited_View (New_P, Renames_Limited_View (Old_P));
         else
            Set_Renamed_Entity (New_P, Old_P);
            Set_Renames_Limited_View (New_P, From_Limited_With (Old_P));
         end if;

         --  The package renaming declaration may become Ghost if it renames a
         --  Ghost entity.

         Mark_Ghost_Renaming (N, Old_P);

         Set_Has_Completion (New_P);
         Set_First_Entity   (New_P, First_Entity (Old_P));
         Set_Last_Entity    (New_P, Last_Entity  (Old_P));
         Set_First_Private_Entity (New_P, First_Private_Entity (Old_P));
         Check_Library_Unit_Renaming (N, Old_P);
         Generate_Reference (Old_P, Name (N));

         --  If the renaming is in the visible part of a package, then we set
         --  Renamed_In_Spec for the renamed package, to prevent giving
         --  warnings about no entities referenced. Such a warning would be
         --  overenthusiastic, since clients can see entities in the renamed
         --  package via the visible package renaming.

         declare
            Ent : constant Entity_Id := Cunit_Entity (Current_Sem_Unit);
         begin
            if Ekind (Ent) = E_Package
              and then not In_Private_Part (Ent)
              and then In_Extended_Main_Source_Unit (N)
              and then Ekind (Old_P) = E_Package
            then
               Set_Renamed_In_Spec (Old_P);
            end if;
         end;

         --  If this is the renaming declaration of a package instantiation
         --  within itself, it is the declaration that ends the list of actuals
         --  for the instantiation. At this point, the subtypes that rename
         --  the actuals are flagged as generic, to avoid spurious ambiguities
         --  if the actuals for two distinct formals happen to coincide. If
         --  the actual is a private type, the subtype has a private completion
         --  that is flagged in the same fashion.

         --  Resolution is identical to what is was in the original generic.
         --  On exit from the generic instance, these are turned into regular
         --  subtypes again, so they are compatible with types in their class.

         if not Is_Generic_Instance (Old_P) then
            return;
         else
            Spec := Specification (Unit_Declaration_Node (Old_P));
         end if;

         if Nkind (Spec) = N_Package_Specification
           and then Present (Generic_Parent (Spec))
           and then Old_P = Current_Scope
           and then Chars (New_P) = Chars (Generic_Parent (Spec))
         then
            declare
               E : Entity_Id;

            begin
               E := First_Entity (Old_P);
               while Present (E) and then E /= New_P loop
                  if Is_Type (E)
                    and then Nkind (Parent (E)) = N_Subtype_Declaration
                  then
                     Set_Is_Generic_Actual_Type (E);

                     if Is_Private_Type (E)
                       and then Present (Full_View (E))
                     then
                        Set_Is_Generic_Actual_Type (Full_View (E));
                     end if;
                  end if;

                  Next_Entity (E);
               end loop;
            end;
         end if;
      end if;

      --  Implementation-defined aspect specifications can appear in a renaming
      --  declaration, but not language-defined ones. The call to procedure
      --  Analyze_Aspect_Specifications will take care of this error check.

      Analyze_Aspect_Specifications (N, New_P);
   end Analyze_Package_Renaming;

   -------------------------------
   -- Analyze_Renamed_Character --
   -------------------------------

   procedure Analyze_Renamed_Character
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean)
   is
      C : constant Node_Id := Name (N);

   begin
      if Ekind (New_S) = E_Function then
         Resolve (C, Etype (New_S));

         if Is_Body then
            Check_Frozen_Renaming (N, New_S);
         end if;

      else
         Error_Msg_N ("character literal can only be renamed as function", N);
      end if;
   end Analyze_Renamed_Character;

   ---------------------------------
   -- Analyze_Renamed_Dereference --
   ---------------------------------

   procedure Analyze_Renamed_Dereference
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean)
   is
      Nam : constant Node_Id := Name (N);
      P   : constant Node_Id := Prefix (Nam);
      Typ : Entity_Id;
      Ind : Interp_Index;
      It  : Interp;

   begin
      if not Is_Overloaded (P) then
         if Ekind (Etype (Nam)) /= E_Subprogram_Type
           or else not Type_Conformant (Etype (Nam), New_S)
         then
            Error_Msg_N ("designated type does not match specification", P);
         else
            Resolve (P);
         end if;

         return;

      else
         Typ := Any_Type;
         Get_First_Interp (Nam, Ind, It);

         while Present (It.Nam) loop

            if Ekind (It.Nam) = E_Subprogram_Type
              and then Type_Conformant (It.Nam, New_S)
            then
               if Typ /= Any_Id then
                  Error_Msg_N ("ambiguous renaming", P);
                  return;
               else
                  Typ := It.Nam;
               end if;
            end if;

            Get_Next_Interp (Ind, It);
         end loop;

         if Typ = Any_Type then
            Error_Msg_N ("designated type does not match specification", P);
         else
            Resolve (N, Typ);

            if Is_Body then
               Check_Frozen_Renaming (N, New_S);
            end if;
         end if;
      end if;
   end Analyze_Renamed_Dereference;

   ---------------------------
   -- Analyze_Renamed_Entry --
   ---------------------------

   procedure Analyze_Renamed_Entry
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean)
   is
      Nam       : constant Node_Id := Name (N);
      Sel       : constant Node_Id := Selector_Name (Nam);
      Is_Actual : constant Boolean := Present (Corresponding_Formal_Spec (N));
      Old_S     : Entity_Id;

   begin
      if Entity (Sel) = Any_Id then

         --  Selector is undefined on prefix. Error emitted already

         Set_Has_Completion (New_S);
         return;
      end if;

      --  Otherwise find renamed entity and build body of New_S as a call to it

      Old_S := Find_Renamed_Entity (N, Selector_Name (Nam), New_S);

      if Old_S = Any_Id then
         Error_Msg_N ("no subprogram or entry matches specification",  N);
      else
         if Is_Body then
            Check_Subtype_Conformant (New_S, Old_S, N);
            Generate_Reference (New_S, Defining_Entity (N), 'b');
            Style.Check_Identifier (Defining_Entity (N), New_S);

         else
            --  Only mode conformance required for a renaming_as_declaration

            Check_Mode_Conformant (New_S, Old_S, N);
         end if;

         Inherit_Renamed_Profile (New_S, Old_S);

         --  The prefix can be an arbitrary expression that yields a task or
         --  protected object, so it must be resolved.

         if Is_Access_Type (Etype (Prefix (Nam))) then
            Insert_Explicit_Dereference (Prefix (Nam));
         end if;
         Resolve (Prefix (Nam), Scope (Old_S));
      end if;

      Set_Convention (New_S, Convention (Old_S));
      Set_Has_Completion (New_S, Inside_A_Generic);

      --  AI05-0225: If the renamed entity is a procedure or entry of a
      --  protected object, the target object must be a variable.

      if Is_Protected_Type (Scope (Old_S))
        and then Ekind (New_S) = E_Procedure
        and then not Is_Variable (Prefix (Nam))
      then
         if Is_Actual then
            Error_Msg_N
              ("target object of protected operation used as actual for "
               & "formal procedure must be a variable", Nam);
         else
            Error_Msg_N
              ("target object of protected operation renamed as procedure, "
               & "must be a variable", Nam);
         end if;
      end if;

      if Is_Body then
         Check_Frozen_Renaming (N, New_S);
      end if;
   end Analyze_Renamed_Entry;

   -----------------------------------
   -- Analyze_Renamed_Family_Member --
   -----------------------------------

   procedure Analyze_Renamed_Family_Member
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean)
   is
      Nam   : constant Node_Id := Name (N);
      P     : constant Node_Id := Prefix (Nam);
      Old_S : Entity_Id;

   begin
      if (Is_Entity_Name (P) and then Ekind (Entity (P)) = E_Entry_Family)
        or else (Nkind (P) = N_Selected_Component
                  and then Ekind (Entity (Selector_Name (P))) = E_Entry_Family)
      then
         if Is_Entity_Name (P) then
            Old_S := Entity (P);
         else
            Old_S := Entity (Selector_Name (P));
         end if;

         if not Entity_Matches_Spec (Old_S, New_S) then
            Error_Msg_N ("entry family does not match specification", N);

         elsif Is_Body then
            Check_Subtype_Conformant (New_S, Old_S, N);
            Generate_Reference (New_S, Defining_Entity (N), 'b');
            Style.Check_Identifier (Defining_Entity (N), New_S);
         end if;

      else
         Error_Msg_N ("no entry family matches specification", N);
      end if;

      Set_Has_Completion (New_S, Inside_A_Generic);

      if Is_Body then
         Check_Frozen_Renaming (N, New_S);
      end if;
   end Analyze_Renamed_Family_Member;

   -----------------------------------------
   -- Analyze_Renamed_Primitive_Operation --
   -----------------------------------------

   procedure Analyze_Renamed_Primitive_Operation
     (N       : Node_Id;
      New_S   : Entity_Id;
      Is_Body : Boolean)
   is
      Old_S : Entity_Id;
      Nam   : Entity_Id;

      function Conforms
        (Subp : Entity_Id;
         Ctyp : Conformance_Type) return Boolean;
      --  Verify that the signatures of the renamed entity and the new entity
      --  match. The first formal of the renamed entity is skipped because it
      --  is the target object in any subsequent call.

      --------------
      -- Conforms --
      --------------

      function Conforms
        (Subp : Entity_Id;
         Ctyp : Conformance_Type) return Boolean
      is
         Old_F : Entity_Id;
         New_F : Entity_Id;

      begin
         if Ekind (Subp) /= Ekind (New_S) then
            return False;
         end if;

         Old_F := Next_Formal (First_Formal (Subp));
         New_F := First_Formal (New_S);
         while Present (Old_F) and then Present (New_F) loop
            if not Conforming_Types (Etype (Old_F), Etype (New_F), Ctyp) then
               return False;
            end if;

            if Ctyp >= Mode_Conformant
              and then Ekind (Old_F) /= Ekind (New_F)
            then
               return False;
            end if;

            Next_Formal (New_F);
            Next_Formal (Old_F);
         end loop;

         return True;
      end Conforms;

   --  Start of processing for Analyze_Renamed_Primitive_Operation

   begin
      if not Is_Overloaded (Selector_Name (Name (N))) then
         Old_S := Entity (Selector_Name (Name (N)));

         if not Conforms (Old_S, Type_Conformant) then
            Old_S := Any_Id;
         end if;

      else
         --  Find the operation that matches the given signature

         declare
            It  : Interp;
            Ind : Interp_Index;

         begin
            Old_S := Any_Id;
            Get_First_Interp (Selector_Name (Name (N)), Ind, It);

            while Present (It.Nam) loop
               if Conforms (It.Nam, Type_Conformant) then
                  Old_S := It.Nam;
               end if;

               Get_Next_Interp (Ind, It);
            end loop;
         end;
      end if;

      if Old_S = Any_Id then
         Error_Msg_N ("no subprogram or entry matches specification",  N);

      else
         if Is_Body then
            if not Conforms (Old_S, Subtype_Conformant) then
               Error_Msg_N ("subtype conformance error in renaming", N);
            end if;

            Generate_Reference (New_S, Defining_Entity (N), 'b');
            Style.Check_Identifier (Defining_Entity (N), New_S);

         else
            --  Only mode conformance required for a renaming_as_declaration

            if not Conforms (Old_S, Mode_Conformant) then
               Error_Msg_N ("mode conformance error in renaming", N);
            end if;

            --  AI12-0204: The prefix of a prefixed view that is renamed or
            --  passed as a formal subprogram must be renamable as an object.

            Nam := Prefix (Name (N));

            if Is_Object_Reference (Nam) then
               if Is_Dependent_Component_Of_Mutable_Object (Nam) then
                  Error_Msg_N
                    ("illegal renaming of discriminant-dependent component",
                     Nam);
               elsif Depends_On_Mutably_Tagged_Ext_Comp (Nam) then
                  Error_Msg_N
                    ("illegal renaming of mutably tagged dependent component",
                     Nam);
               end if;
            else
               Error_Msg_N ("expect object name in renaming", Nam);
            end if;

            --  Enforce the rule given in (RM 6.3.1 (10.1/2)): a prefixed
            --  view of a subprogram is intrinsic, because the compiler has
            --  to generate a wrapper for any call to it. If the name in a
            --  subprogram renaming is a prefixed view, the entity is thus
            --  intrinsic, and 'Access cannot be applied to it.

            Set_Convention (New_S, Convention_Intrinsic);
         end if;

         --  Inherit_Renamed_Profile (New_S, Old_S);

         --  The prefix can be an arbitrary expression that yields an
         --  object, so it must be resolved.

         Resolve (Prefix (Name (N)));
      end if;
   end Analyze_Renamed_Primitive_Operation;

   ---------------------------------
   -- Analyze_Subprogram_Renaming --
   ---------------------------------

   procedure Analyze_Subprogram_Renaming (N : Node_Id) is
      Formal_Spec : constant Entity_Id        := Corresponding_Formal_Spec (N);
      Is_Actual   : constant Boolean          := Present (Formal_Spec);
      Nam         : constant Node_Id          := Name (N);
      Save_AV     : constant Ada_Version_Type := Ada_Version;
      Save_AVP    : constant Node_Id          := Ada_Version_Pragma;
      Save_AV_Exp : constant Ada_Version_Type := Ada_Version_Explicit;
      Spec        : constant Node_Id          := Specification (N);

      Old_S       : Entity_Id := Empty;
      Rename_Spec : Entity_Id;

      procedure Check_Null_Exclusion
        (Ren : Entity_Id;
         Sub : Entity_Id);
      --  Ada 2005 (AI-423): Given renaming Ren of subprogram Sub, check the
      --  following AI rules:
      --
      --    If Ren denotes a generic formal object of a generic unit G, and the
      --    renaming (or instantiation containing the actual) occurs within the
      --    body of G or within the body of a generic unit declared within the
      --    declarative region of G, then the corresponding parameter of G
      --    shall have a null_exclusion; Otherwise the subtype of the Sub's
      --    formal parameter shall exclude null.
      --
      --    Similarly for its return profile.

      procedure Check_SPARK_Primitive_Operation (Subp_Id : Entity_Id);
      --  Ensure that a SPARK renaming denoted by its entity Subp_Id does not
      --  declare a primitive operation of a tagged type (SPARK RM 6.1.1(3)).

      procedure Freeze_Actual_Profile;
      --  In Ada 2012, enforce the freezing rule concerning formal incomplete
      --  types: a callable entity freezes its profile, unless it has an
      --  incomplete untagged formal (RM 13.14(10.2/3)).

      function Has_Class_Wide_Actual return Boolean;
      --  Ada 2012 (AI05-071, AI05-0131) and Ada 2022 (AI12-0165): True if N is
      --  the renaming for a defaulted formal subprogram where the actual for
      --  the controlling formal type is class-wide.

      procedure Handle_Instance_With_Class_Wide_Type
        (Inst_Node    : Node_Id;
         Ren_Id       : Entity_Id;
         Wrapped_Prim : out Entity_Id;
         Wrap_Id      : out Entity_Id);
      --  Ada 2012 (AI05-0071), Ada 2022 (AI12-0165): when the actual type
      --  of an instantiation is a class-wide type T'Class we may need to
      --  wrap a primitive operation of T; this routine looks for a suitable
      --  primitive to be wrapped and (if the wrapper is required) returns the
      --  Id of the wrapped primitive and the Id of the built wrapper. Ren_Id
      --  is the defining entity for the renamed subprogram specification.

      function Original_Subprogram (Subp : Entity_Id) return Entity_Id;
      --  Find renamed entity when the declaration is a renaming_as_body and
      --  the renamed entity may itself be a renaming_as_body. Used to enforce
      --  rule that a renaming_as_body is illegal if the declaration occurs
      --  before the subprogram it completes is frozen, and renaming indirectly
      --  renames the subprogram itself.(Defect Report 8652/0027).

      --------------------------
      -- Check_Null_Exclusion --
      --------------------------

      procedure Check_Null_Exclusion
        (Ren : Entity_Id;
         Sub : Entity_Id)
      is
         Ren_Formal : Entity_Id;
         Sub_Formal : Entity_Id;

         function Null_Exclusion_Mismatch
           (Renaming : Entity_Id; Renamed : Entity_Id) return Boolean;
         --  Return True if there is a null exclusion mismatch between
         --  Renaming and Renamed, False otherwise.

         -----------------------------
         -- Null_Exclusion_Mismatch --
         -----------------------------

         function Null_Exclusion_Mismatch
           (Renaming : Entity_Id; Renamed : Entity_Id) return Boolean is
         begin
            return Has_Null_Exclusion (Parent (Renaming))
              and then
                not (Has_Null_Exclusion (Parent (Renamed))
                      or else (Can_Never_Be_Null (Etype (Renamed))
                                and then not
                                  (Is_Formal_Subprogram (Sub)
                                   and then In_Generic_Body (Current_Scope))));
         end Null_Exclusion_Mismatch;

      begin
         --  Parameter check

         Ren_Formal := First_Formal (Ren);
         Sub_Formal := First_Formal (Sub);
         while Present (Ren_Formal) and then Present (Sub_Formal) loop
            if Null_Exclusion_Mismatch (Ren_Formal, Sub_Formal) then
               Error_Msg_Sloc := Sloc (Sub_Formal);
               Error_Msg_NE
                 ("`NOT NULL` required for parameter &#",
                  Ren_Formal, Sub_Formal);
            end if;

            Next_Formal (Ren_Formal);
            Next_Formal (Sub_Formal);
         end loop;

         --  Return profile check

         if Nkind (Parent (Ren)) = N_Function_Specification
           and then Nkind (Parent (Sub)) = N_Function_Specification
           and then Null_Exclusion_Mismatch (Ren, Sub)
         then
            Error_Msg_Sloc := Sloc (Sub);
            Error_Msg_N ("return must specify `NOT NULL`#", Ren);
         end if;
      end Check_Null_Exclusion;

      -------------------------------------
      -- Check_SPARK_Primitive_Operation --
      -------------------------------------

      procedure Check_SPARK_Primitive_Operation (Subp_Id : Entity_Id) is
         Prag : constant Node_Id := SPARK_Pragma (Subp_Id);
         Typ  : Entity_Id;

      begin
         --  Nothing to do when the subprogram is not subject to SPARK_Mode On
         --  because this check applies to SPARK code only.

         if not (Present (Prag)
                  and then Get_SPARK_Mode_From_Annotation (Prag) = On)
         then
            return;

         --  Nothing to do when the subprogram is not a primitive operation

         elsif not Is_Primitive (Subp_Id) then
            return;
         end if;

         Typ := Find_Dispatching_Type (Subp_Id);

         --  Nothing to do when the subprogram is a primitive operation of an
         --  untagged type.

         if No (Typ) then
            return;
         end if;

         --  At this point a renaming declaration introduces a new primitive
         --  operation for a tagged type.

         Error_Msg_Node_2 := Typ;
         Error_Msg_NE
           ("subprogram renaming & cannot declare primitive for type & "
            & "(SPARK RM 6.1.1(3))", N, Subp_Id);
      end Check_SPARK_Primitive_Operation;

      ---------------------------
      -- Freeze_Actual_Profile --
      ---------------------------

      procedure Freeze_Actual_Profile is
         F                  : Entity_Id;
         Has_Untagged_Inc   : Boolean;
         Instantiation_Node : constant Node_Id := Parent (N);

      begin
         if Ada_Version >= Ada_2012 then
            F := First_Formal (Formal_Spec);
            Has_Untagged_Inc := False;
            while Present (F) loop
               if Ekind (Etype (F)) = E_Incomplete_Type
                 and then not Is_Tagged_Type (Etype (F))
               then
                  Has_Untagged_Inc := True;
                  exit;
               end if;

               Next_Formal (F);
            end loop;

            if Ekind (Formal_Spec) = E_Function
              and then not Is_Tagged_Type (Etype (Formal_Spec))
            then
               Has_Untagged_Inc := True;
            end if;

            if not Has_Untagged_Inc then
               F := First_Formal (Old_S);
               while Present (F) loop
                  Freeze_Before (Instantiation_Node, Etype (F));

                  if Is_Incomplete_Or_Private_Type (Etype (F))
                    and then No (Underlying_Type (Etype (F)))
                  then
                     --  Exclude generic types, or types derived  from them.
                     --  They will be frozen in the enclosing instance.

                     if Is_Generic_Type (Etype (F))
                       or else Is_Generic_Type (Root_Type (Etype (F)))
                     then
                        null;

                     --  A limited view of a type declared elsewhere needs no
                     --  freezing actions.

                     elsif From_Limited_With (Etype (F)) then
                        null;

                     --  Incomplete types are never frozen (AI12-0155-1)

                     elsif Is_Incomplete_Type (Etype (F)) then
                        null;

                     else
                        Error_Msg_NE
                          ("type& must be frozen before this point",
                           Instantiation_Node, Etype (F));
                     end if;
                  end if;

                  Next_Formal (F);
               end loop;
            end if;
         end if;
      end Freeze_Actual_Profile;

      ---------------------------
      -- Has_Class_Wide_Actual --
      ---------------------------

      function Has_Class_Wide_Actual return Boolean is
         Formal     : Entity_Id;
         Formal_Typ : Entity_Id;

      begin
         if Is_Actual then
            Formal := First_Formal (Formal_Spec);
            while Present (Formal) loop
               Formal_Typ := Etype (Formal);

               if Has_Unknown_Discriminants (Formal_Typ)
                 and then not Is_Class_Wide_Type (Formal_Typ)
                 and then Is_Class_Wide_Type (Get_Instance_Of (Formal_Typ))
               then
                  return True;
               end if;

               Next_Formal (Formal);
            end loop;
         end if;

         return False;
      end Has_Class_Wide_Actual;

      ------------------------------------------
      -- Handle_Instance_With_Class_Wide_Type --
      ------------------------------------------

      procedure Handle_Instance_With_Class_Wide_Type
        (Inst_Node    : Node_Id;
         Ren_Id       : Entity_Id;
         Wrapped_Prim : out Entity_Id;
         Wrap_Id      : out Entity_Id)
      is
         procedure Build_Class_Wide_Wrapper
           (Ren_Id  : Entity_Id;
            Prim_Op : Entity_Id;
            Wrap_Id : out Entity_Id);
         --  Build a wrapper for the renaming Ren_Id of subprogram Prim_Op.

         procedure Find_Suitable_Candidate
           (Prim_Op    : out Entity_Id;
            Is_CW_Prim : out Boolean);
         --  Look for a suitable primitive to be wrapped (Prim_Op); Is_CW_Prim
         --  indicates that the found candidate is a class-wide primitive (to
         --  help the caller decide if the wrapper is required).

         ------------------------------
         -- Build_Class_Wide_Wrapper --
         ------------------------------

         procedure Build_Class_Wide_Wrapper
           (Ren_Id  : Entity_Id;
            Prim_Op : Entity_Id;
            Wrap_Id : out Entity_Id)
         is
            Loc : constant Source_Ptr := Sloc (N);

            function Build_Call
              (Subp_Id : Entity_Id;
               Params  : List_Id) return Node_Id;
            --  Create a dispatching call to invoke routine Subp_Id with
            --  actuals built from the parameter specifications of list Params.

            function Build_Expr_Fun_Call
              (Subp_Id : Entity_Id;
               Params  : List_Id) return Node_Id;
            --  Create a dispatching call to invoke function Subp_Id with
            --  actuals built from the parameter specifications of list Params.
            --  Directly return the call, so that it can be used inside an
            --  expression function. This is a requirement of GNATprove mode.

            function Build_Spec (Subp_Id : Entity_Id) return Node_Id;
            --  Create a subprogram specification based on the subprogram
            --  profile of Subp_Id.

            ----------------
            -- Build_Call --
            ----------------

            function Build_Call
              (Subp_Id : Entity_Id;
               Params  : List_Id) return Node_Id
            is
               Actuals  : constant List_Id := New_List;
               Call_Ref : constant Node_Id := New_Occurrence_Of (Subp_Id, Loc);
               Formal   : Node_Id;

            begin
               --  Build the actual parameters of the call

               Formal := First (Params);
               while Present (Formal) loop
                  Append_To (Actuals,
                    Make_Identifier (Loc,
                      Chars (Defining_Identifier (Formal))));
                  Next (Formal);
               end loop;

               --  Generate:
               --    return Subp_Id (Actuals);

               if Ekind (Subp_Id) in E_Function | E_Operator then
                  return
                    Make_Simple_Return_Statement (Loc,
                      Expression =>
                        Make_Function_Call (Loc,
                          Name                   => Call_Ref,
                          Parameter_Associations => Actuals));

               --  Generate:
               --    Subp_Id (Actuals);

               else
                  return
                    Make_Procedure_Call_Statement (Loc,
                      Name                   => Call_Ref,
                      Parameter_Associations => Actuals);
               end if;
            end Build_Call;

            -------------------------
            -- Build_Expr_Fun_Call --
            -------------------------

            function Build_Expr_Fun_Call
              (Subp_Id : Entity_Id;
               Params  : List_Id) return Node_Id
            is
               Actuals  : constant List_Id := New_List;
               Call_Ref : constant Node_Id := New_Occurrence_Of (Subp_Id, Loc);
               Formal   : Node_Id;

            begin
               pragma Assert (Ekind (Subp_Id) in E_Function | E_Operator);

               --  Build the actual parameters of the call

               Formal := First (Params);
               while Present (Formal) loop
                  Append_To (Actuals,
                    Make_Identifier (Loc,
                      Chars (Defining_Identifier (Formal))));
                  Next (Formal);
               end loop;

               --  Generate:
               --    Subp_Id (Actuals);

               return
                 Make_Function_Call (Loc,
                   Name                   => Call_Ref,
                   Parameter_Associations => Actuals);
            end Build_Expr_Fun_Call;

            ----------------
            -- Build_Spec --
            ----------------

            function Build_Spec (Subp_Id : Entity_Id) return Node_Id is
               Params  : constant List_Id   := Copy_Parameter_List (Subp_Id);
               Spec_Id : constant Entity_Id :=
                           Make_Defining_Identifier (Loc,
                             New_External_Name (Chars (Subp_Id), 'R'));

            begin
               if Ekind (Formal_Spec) = E_Procedure then
                  return
                    Make_Procedure_Specification (Loc,
                      Defining_Unit_Name       => Spec_Id,
                      Parameter_Specifications => Params);
               else
                  return
                    Make_Function_Specification (Loc,
                      Defining_Unit_Name       => Spec_Id,
                      Parameter_Specifications => Params,
                      Result_Definition =>
                        New_Copy_Tree (Result_Definition (Spec)));
               end if;
            end Build_Spec;

            --  Local variables

            Body_Decl : Node_Id;
            Spec_Decl : Node_Id;
            New_Spec  : Node_Id;

         --  Start of processing for Build_Class_Wide_Wrapper

         begin
            pragma Assert (not Error_Posted (Nam));

            --  Step 1: Create the declaration and the body of the wrapper,
            --  insert all the pieces into the tree.

            --  In GNATprove mode, create a function wrapper in the form of an
            --  expression function, so that an implicit postcondition relating
            --  the result of calling the wrapper function and the result of
            --  the dispatching call to the wrapped function is known during
            --  proof.

            if GNATprove_Mode
              and then Ekind (Ren_Id) in E_Function | E_Operator
            then
               New_Spec := Build_Spec (Ren_Id);
               Body_Decl :=
                 Make_Expression_Function (Loc,
                   Specification => New_Spec,
                   Expression    =>
                     Build_Expr_Fun_Call
                       (Subp_Id => Prim_Op,
                        Params  => Parameter_Specifications (New_Spec)));

               Wrap_Id := Defining_Entity (Body_Decl);

            --  Otherwise, create separate spec and body for the subprogram

            else
               Spec_Decl :=
                 Make_Subprogram_Declaration (Loc,
                   Specification => Build_Spec (Ren_Id));
               Insert_Before_And_Analyze (N, Spec_Decl);

               Wrap_Id := Defining_Entity (Spec_Decl);

               Body_Decl :=
                 Make_Subprogram_Body (Loc,
                   Specification              => Build_Spec (Ren_Id),
                   Declarations               => New_List,
                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List (
                         Build_Call
                           (Subp_Id => Prim_Op,
                            Params  =>
                              Parameter_Specifications
                                (Specification (Spec_Decl))))));

               Set_Corresponding_Body (Spec_Decl, Defining_Entity (Body_Decl));
            end if;

            Set_Is_Class_Wide_Wrapper (Wrap_Id);

            --  If the operator carries an Eliminated pragma, indicate that
            --  the wrapper is also to be eliminated, to prevent spurious
            --  errors when using gnatelim on programs that include box-
            --  defaulted initialization of equality operators.

            Set_Is_Eliminated (Wrap_Id, Is_Eliminated (Prim_Op));

            --  In GNATprove mode, insert the body in the tree for analysis

            if GNATprove_Mode then
               Insert_Before_And_Analyze (N, Body_Decl);
            end if;

            --  The generated body does not freeze and must be analyzed when
            --  the class-wide wrapper is frozen. The body is only needed if
            --  expansion is enabled.

            if Expander_Active then
               Append_Freeze_Action (Wrap_Id, Body_Decl);
            end if;

            --  Step 2: The subprogram renaming aliases the wrapper

            Rewrite (Name (N), New_Occurrence_Of (Wrap_Id, Loc));
         end Build_Class_Wide_Wrapper;

         -----------------------------
         -- Find_Suitable_Candidate --
         -----------------------------

         procedure Find_Suitable_Candidate
           (Prim_Op    : out Entity_Id;
            Is_CW_Prim : out Boolean)
         is
            Loc : constant Source_Ptr := Sloc (N);

            function Find_Primitive (Typ : Entity_Id) return Entity_Id;
            --  Find a primitive subprogram of type Typ which matches the
            --  profile of the renaming declaration.

            procedure Interpretation_Error (Subp_Id : Entity_Id);
            --  Emit a continuation error message suggesting subprogram Subp_Id
            --  as a possible interpretation.

            function Is_Intrinsic_Equality
              (Subp_Id : Entity_Id) return Boolean;
            --  Determine whether subprogram Subp_Id denotes the intrinsic "="
            --  operator.

            function Is_Suitable_Candidate
              (Subp_Id : Entity_Id) return Boolean;
            --  Determine whether subprogram Subp_Id is a suitable candidate
            --  for the role of a wrapped subprogram.

            --------------------
            -- Find_Primitive --
            --------------------

            function Find_Primitive (Typ : Entity_Id) return Entity_Id is
               procedure Replace_Parameter_Types (Spec : Node_Id);
               --  Given a specification Spec, replace all class-wide parameter
               --  types with reference to type Typ.

               -----------------------------
               -- Replace_Parameter_Types --
               -----------------------------

               procedure Replace_Parameter_Types (Spec : Node_Id) is
                  Formal     : Node_Id;
                  Formal_Id  : Entity_Id;
                  Formal_Typ : Node_Id;

               begin
                  Formal := First (Parameter_Specifications (Spec));
                  while Present (Formal) loop
                     Formal_Id  := Defining_Identifier (Formal);
                     Formal_Typ := Parameter_Type (Formal);

                     --  Create a new entity for each class-wide formal to
                     --  prevent aliasing with the original renaming. Replace
                     --  the type of such a parameter with the candidate type.

                     if Nkind (Formal_Typ) = N_Identifier
                       and then Is_Class_Wide_Type (Etype (Formal_Typ))
                     then
                        Set_Defining_Identifier (Formal,
                          Make_Defining_Identifier (Loc, Chars (Formal_Id)));

                        Set_Parameter_Type (Formal,
                          New_Occurrence_Of (Typ, Loc));
                     end if;

                     Next (Formal);
                  end loop;
               end Replace_Parameter_Types;

               --  Local variables

               Alt_Ren  : constant Node_Id := New_Copy_Tree (N);
               Alt_Nam  : constant Node_Id := Name (Alt_Ren);
               Alt_Spec : constant Node_Id := Specification (Alt_Ren);
               Subp_Id  : Entity_Id;

            --  Start of processing for Find_Primitive

            begin
               --  Each attempt to find a suitable primitive of a particular
               --  type operates on its own copy of the original renaming.
               --  As a result the original renaming is kept decoration and
               --  side-effect free.

               --  Inherit the overloaded status of the renamed subprogram name

               if Is_Overloaded (Nam) then
                  Set_Is_Overloaded (Alt_Nam);
                  Save_Interps (Nam, Alt_Nam);
               end if;

               --  The copied renaming is hidden from visibility to prevent the
               --  pollution of the enclosing context.

               Set_Defining_Unit_Name (Alt_Spec, Make_Temporary (Loc, 'R'));

               --  The types of all class-wide parameters must be changed to
               --  the candidate type.

               Replace_Parameter_Types (Alt_Spec);

               --  Try to find a suitable primitive that matches the altered
               --  profile of the renaming specification.

               Subp_Id :=
                 Find_Renamed_Entity
                   (N         => Alt_Ren,
                    Nam       => Name (Alt_Ren),
                    New_S     => Analyze_Subprogram_Specification (Alt_Spec),
                    Is_Actual => Is_Actual);

               --  Do not return Any_Id if the resolution of the altered
               --  profile failed as this complicates further checks on
               --  the caller side; return Empty instead.

               if Subp_Id = Any_Id then
                  return Empty;
               else
                  return Subp_Id;
               end if;
            end Find_Primitive;

            --------------------------
            -- Interpretation_Error --
            --------------------------

            procedure Interpretation_Error (Subp_Id : Entity_Id) is
            begin
               Error_Msg_Sloc := Sloc (Subp_Id);

               if Is_Internal (Subp_Id) then
                  Error_Msg_NE
                    ("\\possible interpretation: predefined & #",
                     Spec, Formal_Spec);
               else
                  Error_Msg_NE
                    ("\\possible interpretation: & defined #",
                     Spec, Formal_Spec);
               end if;
            end Interpretation_Error;

            ---------------------------
            -- Is_Intrinsic_Equality --
            ---------------------------

            function Is_Intrinsic_Equality (Subp_Id : Entity_Id) return Boolean
            is
            begin
               return
                 Ekind (Subp_Id) = E_Operator
                   and then Chars (Subp_Id) = Name_Op_Eq
                   and then Is_Intrinsic_Subprogram (Subp_Id);
            end Is_Intrinsic_Equality;

            ---------------------------
            -- Is_Suitable_Candidate --
            ---------------------------

            function Is_Suitable_Candidate (Subp_Id : Entity_Id) return Boolean
            is
            begin
               if No (Subp_Id) then
                  return False;

               --  An intrinsic subprogram is never a good candidate. This
               --  is an indication of a missing primitive, either defined
               --  directly or inherited from a parent tagged type.

               elsif Is_Intrinsic_Subprogram (Subp_Id) then
                  return False;

               else
                  return True;
               end if;
            end Is_Suitable_Candidate;

            --  Local variables

            Actual_Typ : Entity_Id := Empty;
            --  The actual class-wide type for Formal_Typ

            CW_Prim_OK : Boolean;
            CW_Prim_Op : Entity_Id;
            --  The class-wide subprogram (if available) that corresponds to
            --  the renamed generic formal subprogram.

            Formal_Typ : Entity_Id := Empty;
            --  The generic formal type with unknown discriminants

            Root_Prim_OK : Boolean;
            Root_Prim_Op : Entity_Id;
            --  The root type primitive (if available) that corresponds to the
            --  renamed generic formal subprogram.

            Root_Typ : Entity_Id := Empty;
            --  The root type of Actual_Typ

            Formal   : Node_Id;

         --  Start of processing for Find_Suitable_Candidate

         begin
            pragma Assert (not Error_Posted (Nam));

            Prim_Op    := Empty;
            Is_CW_Prim := False;

            --  Analyze the renamed name, but do not resolve it. The resolution
            --  is completed once a suitable subprogram is found.

            Analyze (Nam);

            --  When the renamed name denotes the intrinsic operator equals,
            --  the name must be treated as overloaded. This allows for a
            --  potential match against the root type's predefined equality
            --  function.

            if Is_Intrinsic_Equality (Entity (Nam)) then
               Set_Is_Overloaded (Nam);
               Collect_Interps   (Nam);
            end if;

            --  Step 1: Find the generic formal type and its corresponding
            --  class-wide actual type from the renamed generic formal
            --  subprogram.

            Formal := First_Formal (Formal_Spec);
            while Present (Formal) loop
               if Has_Unknown_Discriminants (Etype (Formal))
                 and then not Is_Class_Wide_Type (Etype (Formal))
                 and then Is_Class_Wide_Type (Get_Instance_Of (Etype (Formal)))
               then
                  Formal_Typ := Etype (Formal);
                  Actual_Typ := Base_Type (Get_Instance_Of (Formal_Typ));
                  Root_Typ   := Root_Type (Actual_Typ);
                  exit;
               end if;

               Next_Formal (Formal);
            end loop;

            --  The specification of the generic formal subprogram should
            --  always contain a formal type with unknown discriminants whose
            --  actual is a class-wide type; otherwise this indicates a failure
            --  in function Has_Class_Wide_Actual.

            pragma Assert (Present (Formal_Typ));

            --  Step 2: Find the proper class-wide subprogram or primitive
            --  that corresponds to the renamed generic formal subprogram.

            CW_Prim_Op   := Find_Primitive (Actual_Typ);
            CW_Prim_OK   := Is_Suitable_Candidate (CW_Prim_Op);
            Root_Prim_Op := Find_Primitive (Root_Typ);
            Root_Prim_OK := Is_Suitable_Candidate (Root_Prim_Op);

            --  The class-wide actual type has two subprograms that correspond
            --  to the renamed generic formal subprogram:

            --    with procedure Prim_Op (Param : Formal_Typ);

            --    procedure Prim_Op (Param : Actual_Typ);  --  may be inherited
            --    procedure Prim_Op (Param : Actual_Typ'Class);

            --  Even though the declaration of the two subprograms is legal, a
            --  call to either one is ambiguous and therefore illegal.

            if CW_Prim_OK and Root_Prim_OK then

               --  A user-defined primitive has precedence over a predefined
               --  one.

               if Is_Internal (CW_Prim_Op)
                 and then not Is_Internal (Root_Prim_Op)
               then
                  Prim_Op := Root_Prim_Op;

               elsif Is_Internal (Root_Prim_Op)
                 and then not Is_Internal (CW_Prim_Op)
               then
                  Prim_Op := CW_Prim_Op;
                  Is_CW_Prim := True;

               elsif CW_Prim_Op = Root_Prim_Op then
                  Prim_Op := Root_Prim_Op;

               --  The two subprograms are legal but the class-wide subprogram
               --  is a class-wide wrapper built for a previous instantiation;
               --  the wrapper has precedence.

               elsif Present (Alias (CW_Prim_Op))
                 and then Is_Class_Wide_Wrapper (Ultimate_Alias (CW_Prim_Op))
               then
                  Prim_Op := CW_Prim_Op;
                  Is_CW_Prim := True;

               --  Otherwise both candidate subprograms are user-defined and
               --  ambiguous.

               else
                  Error_Msg_NE
                    ("ambiguous actual for generic subprogram &",
                     Spec, Formal_Spec);
                  Interpretation_Error (Root_Prim_Op);
                  Interpretation_Error (CW_Prim_Op);
                  return;
               end if;

            elsif CW_Prim_OK and not Root_Prim_OK then
               Prim_Op := CW_Prim_Op;
               Is_CW_Prim := True;

            elsif not CW_Prim_OK and Root_Prim_OK then
               Prim_Op := Root_Prim_Op;

            --  An intrinsic equality may act as a suitable candidate in the
            --  case of a null type extension where the parent's equality
            --  is hidden. A call to an intrinsic equality is expanded as
            --  dispatching.

            elsif Present (Root_Prim_Op)
              and then Is_Intrinsic_Equality (Root_Prim_Op)
            then
               Prim_Op := Root_Prim_Op;

            --  Otherwise there are no candidate subprograms. Let the caller
            --  diagnose the error.

            else
               return;
            end if;

            --  At this point resolution has taken place and the name is no
            --  longer overloaded. Mark the primitive as referenced.

            Set_Is_Overloaded (Name (N), False);
            Set_Referenced    (Prim_Op);
         end Find_Suitable_Candidate;

         --  Local variables

         Is_CW_Prim : Boolean;

      --  Start of processing for Handle_Instance_With_Class_Wide_Type

      begin
         Wrapped_Prim := Empty;
         Wrap_Id := Empty;

         --  Ada 2012 (AI05-0071): A generic/instance scenario involving a
         --  formal type with unknown discriminants and a generic primitive
         --  operation of the said type with a box require special processing
         --  when the actual is a class-wide type:
         --
         --    generic
         --       type Formal_Typ (<>) is private;
         --       with procedure Prim_Op (Param : Formal_Typ) is <>;
         --    package Gen is ...
         --
         --    package Inst is new Gen (Actual_Typ'Class);
         --
         --  In this case the general renaming mechanism used in the prologue
         --  of an instance no longer applies:
         --
         --    procedure Prim_Op (Param : Formal_Typ) renames Prim_Op;
         --
         --  The above is replaced the following wrapper/renaming combination:
         --
         --    procedure Wrapper (Param : Formal_Typ) is  --  wrapper
         --    begin
         --       Prim_Op (Param);                        --  primitive
         --    end Wrapper;
         --
         --    procedure Prim_Op (Param : Formal_Typ) renames Wrapper;
         --
         --  This transformation applies only if there is no explicit visible
         --  class-wide operation at the point of the instantiation. Ren_Id is
         --  the entity of the renaming declaration. When the transformation
         --  applies, Wrapped_Prim is the entity of the wrapped primitive.

         if Box_Present (Inst_Node) then
            Find_Suitable_Candidate
              (Prim_Op    => Wrapped_Prim,
               Is_CW_Prim => Is_CW_Prim);

            if Present (Wrapped_Prim) then
               if not Is_CW_Prim then
                  Build_Class_Wide_Wrapper (Ren_Id, Wrapped_Prim, Wrap_Id);

               --  Small optimization: When the candidate is a class-wide
               --  subprogram we don't build the wrapper; we modify the
               --  renaming declaration to directly map the actual to the
               --  generic formal and discard the candidate.

               else
                  Rewrite (Nam, New_Occurrence_Of (Wrapped_Prim, Sloc (N)));
                  Wrapped_Prim := Empty;
               end if;
            end if;

         --  Ada 2022 (AI12-0165, RM 12.6(8.5/3)): The actual subprogram for a
         --  formal_abstract_subprogram_declaration shall be:
         --   a) a dispatching operation of the controlling type; or
         --   b) if the controlling type is a formal type, and the actual
         --      type corresponding to that formal type is a specific type T,
         --      a dispatching operation of type T; or
         --   c) if the controlling type is a formal type, and the actual
         --      type is a class-wide type T'Class, an implicitly declared
         --      subprogram corresponding to a primitive operation of type T.

         elsif Nkind (Inst_Node) = N_Formal_Abstract_Subprogram_Declaration
           and then Is_Entity_Name (Nam)
         then
            Find_Suitable_Candidate
              (Prim_Op    => Wrapped_Prim,
               Is_CW_Prim => Is_CW_Prim);

            if Present (Wrapped_Prim) then

               --  Cases (a) and (b); see previous description.

               if not Is_CW_Prim then
                  Build_Class_Wide_Wrapper (Ren_Id, Wrapped_Prim, Wrap_Id);

               --  Case (c); see previous description.

               --  Implicit operations of T'Class for subtype declarations
               --  are built by Derive_Subprogram, and their Alias attribute
               --  references the primitive operation of T.

               elsif not Comes_From_Source (Wrapped_Prim)
                 and then Nkind (Parent (Wrapped_Prim)) = N_Subtype_Declaration
                 and then Present (Alias (Wrapped_Prim))
               then
                  --  We don't need to build the wrapper; we modify the
                  --  renaming declaration to directly map the actual to
                  --  the generic formal and discard the candidate.

                  Rewrite (Nam,
                    New_Occurrence_Of (Alias (Wrapped_Prim), Sloc (N)));
                  Wrapped_Prim := Empty;

               --  Legality rules do not apply; discard the candidate.

               else
                  Wrapped_Prim := Empty;
               end if;
            end if;
         end if;
      end Handle_Instance_With_Class_Wide_Type;

      -------------------------
      -- Original_Subprogram --
      -------------------------

      function Original_Subprogram (Subp : Entity_Id) return Entity_Id is
         Orig_Decl : Node_Id;
         Orig_Subp : Entity_Id;

      begin
         --  First case: renamed entity is itself a renaming

         if Present (Alias (Subp)) then
            return Alias (Subp);

         elsif Nkind (Unit_Declaration_Node (Subp)) = N_Subprogram_Declaration
           and then Present (Corresponding_Body (Unit_Declaration_Node (Subp)))
         then
            --  Check if renamed entity is a renaming_as_body

            Orig_Decl :=
              Unit_Declaration_Node
                (Corresponding_Body (Unit_Declaration_Node (Subp)));

            if Nkind (Orig_Decl) = N_Subprogram_Renaming_Declaration then
               Orig_Subp := Entity (Name (Orig_Decl));

               if Orig_Subp = Rename_Spec then

                  --  Circularity detected

                  return Orig_Subp;

               else
                  return (Original_Subprogram (Orig_Subp));
               end if;
            else
               return Subp;
            end if;
         else
            return Subp;
         end if;
      end Original_Subprogram;

      --  Local variables

      CW_Actual : constant Boolean := Has_Class_Wide_Actual;
      --  Ada 2012 (AI05-071, AI05-0131) and Ada 2022 (AI12-0165): True if the
      --  renaming is for a defaulted formal subprogram when the actual for a
      --  related formal type is class-wide.

      Inst_Node    : Node_Id   := Empty;
      New_S        : Entity_Id := Empty;
      Wrapped_Prim : Entity_Id := Empty;

   --  Start of processing for Analyze_Subprogram_Renaming

   begin
      --  We must test for the attribute renaming case before the Analyze
      --  call because otherwise Sem_Attr will complain that the attribute
      --  is missing an argument when it is analyzed.

      if Nkind (Nam) = N_Attribute_Reference then

         --  In the case of an abstract formal subprogram association, rewrite
         --  an actual given by a stream or Put_Image attribute as the name of
         --  the corresponding stream or Put_Image primitive of the type.

         --  In a generic context the stream and Put_Image operations are not
         --  generated, and this must be treated as a normal attribute
         --  reference, to be expanded in subsequent instantiations.

         if Is_Actual
           and then Is_Abstract_Subprogram (Formal_Spec)
           and then Expander_Active
         then
            declare
               Prefix_Type : constant Entity_Id := Entity (Prefix (Nam));
               Prim : Entity_Id;

            begin
               --  The class-wide forms of the stream and Put_Image attributes
               --  are not primitive dispatching operations (even though they
               --  internally dispatch).

               if Is_Class_Wide_Type (Prefix_Type) then
                  Error_Msg_N
                    ("attribute must be a primitive dispatching operation",
                     Nam);
                  return;
               end if;

               --  Retrieve the primitive subprogram associated with the
               --  attribute. This can only be a stream attribute, since those
               --  are the only ones that are dispatching (and the actual for
               --  an abstract formal subprogram must be dispatching
               --  operation).

               case Attribute_Name (Nam) is
                  when Name_Input =>
                     Prim :=
                       Find_Optional_Prim_Op (Prefix_Type, TSS_Stream_Input);

                  when Name_Output =>
                     Prim :=
                       Find_Optional_Prim_Op (Prefix_Type, TSS_Stream_Output);

                  when Name_Read =>
                     Prim :=
                       Find_Optional_Prim_Op (Prefix_Type, TSS_Stream_Read);

                  when Name_Write =>
                     Prim :=
                       Find_Optional_Prim_Op (Prefix_Type, TSS_Stream_Write);

                  when Name_Put_Image =>
                     Prim :=
                       Find_Optional_Prim_Op (Prefix_Type, TSS_Put_Image);

                  when others =>
                     Error_Msg_N
                       ("attribute must be a primitive dispatching operation",
                        Nam);
                     return;
               end case;

               --  If no stream operation was found, and the type is limited,
               --  the user should have defined one. This rule does not apply
               --  to Put_Image.

               if No (Prim)
                 and then Attribute_Name (Nam) /= Name_Put_Image
               then
                  if Is_Limited_Type (Prefix_Type) then
                     Error_Msg_NE
                      ("stream operation not defined for type&",
                        N, Prefix_Type);
                     return;

                  --  Otherwise, compiler should have generated default

                  else
                     raise Program_Error;
                  end if;
               end if;

               --  Rewrite the attribute into the name of its corresponding
               --  primitive dispatching subprogram. We can then proceed with
               --  the usual processing for subprogram renamings.

               declare
                  Prim_Name : constant Node_Id :=
                                Make_Identifier (Sloc (Nam),
                                  Chars => Chars (Prim));
               begin
                  Set_Entity (Prim_Name, Prim);
                  Rewrite (Nam, Prim_Name);
                  Analyze (Nam);
               end;
            end;

         --  Normal processing for a renaming of an attribute

         else
            Attribute_Renaming (N);
            return;
         end if;
      end if;

      --  Check whether this declaration corresponds to the instantiation of a
      --  formal subprogram.

      --  If this is an instantiation, the corresponding actual is frozen and
      --  error messages can be made more precise. If this is a default
      --  subprogram, the entity is already established in the generic, and is
      --  not retrieved by visibility. If it is a default with a box, the
      --  candidate interpretations, if any, have been collected when building
      --  the renaming declaration. If overloaded, the proper interpretation is
      --  determined in Find_Renamed_Entity. If the entity is an operator,
      --  Find_Renamed_Entity applies additional visibility checks.

      if Is_Actual then
         Inst_Node := Unit_Declaration_Node (Formal_Spec);

         --  Ada 2012 (AI05-0071) and Ada 2022 (AI12-0165): when the actual
         --  type is a class-wide type T'Class we may need to wrap a primitive
         --  operation of T. Search for the wrapped primitive and (if required)
         --  build a wrapper whose body consists of a dispatching call to the
         --  wrapped primitive of T, with its formal parameters as the actual
         --  parameters.

         if CW_Actual and then

         --  Ada 2012 (AI05-0071): Check whether the renaming is for a
         --  defaulted actual subprogram with a class-wide actual.

            (Box_Present (Inst_Node)

                or else

         --  Ada 2022 (AI12-0165): Check whether the renaming is for a formal
         --  abstract subprogram declaration with a class-wide actual.

             (Nkind (Inst_Node) = N_Formal_Abstract_Subprogram_Declaration
               and then Is_Entity_Name (Nam)))
         then
            New_S := Analyze_Subprogram_Specification (Spec);

            --  Do not attempt to build the wrapper if the renaming is in error

            if not Error_Posted (Nam) then
               Handle_Instance_With_Class_Wide_Type
                 (Inst_Node    => Inst_Node,
                  Ren_Id       => New_S,
                  Wrapped_Prim => Wrapped_Prim,
                  Wrap_Id      => Old_S);

               --  If several candidates were found, then we reported the
               --  ambiguity; stop processing the renaming declaration to
               --  avoid reporting further (spurious) errors.

               if Error_Posted (Spec) then
                  return;
               end if;

            end if;
         end if;

         if Present (Wrapped_Prim) then

            --  When the wrapper is built, the subprogram renaming aliases
            --  the wrapper.

            Analyze (Nam);

            pragma Assert (Old_S = Entity (Nam)
              and then Is_Class_Wide_Wrapper (Old_S));

            --  The subprogram renaming declaration may become Ghost if it
            --  renames a wrapper of a Ghost entity.

            Mark_Ghost_Renaming (N, Wrapped_Prim);

         elsif Is_Entity_Name (Nam)
           and then Present (Entity (Nam))
           and then not Comes_From_Source (Nam)
           and then not Is_Overloaded (Nam)
         then
            Old_S := Entity (Nam);

            --  The subprogram renaming declaration may become Ghost if it
            --  renames a Ghost entity.

            Mark_Ghost_Renaming (N, Old_S);

            New_S := Analyze_Subprogram_Specification (Spec);

            --  Operator case

            if Ekind (Old_S) = E_Operator then

               --  Box present

               if Box_Present (Inst_Node) then
                  Old_S := Find_Renamed_Entity (N, Name (N), New_S, Is_Actual);

               --  If there is an immediately visible homonym of the operator
               --  and the declaration has a default, this is worth a warning
               --  because the user probably did not intend to get the pre-
               --  defined operator, visible in the generic declaration. To
               --  find if there is an intended candidate, analyze the renaming
               --  again in the current context.

               elsif Scope (Old_S) = Standard_Standard
                 and then Present (Default_Name (Inst_Node))
               then
                  declare
                     Decl   : constant Node_Id := New_Copy_Tree (N);
                     Hidden : Entity_Id;

                  begin
                     Set_Entity (Name (Decl), Empty);
                     Analyze (Name (Decl));
                     Hidden :=
                       Find_Renamed_Entity (Decl, Name (Decl), New_S, True);

                     if Present (Hidden)
                       and then In_Open_Scopes (Scope (Hidden))
                       and then Is_Immediately_Visible (Hidden)
                       and then Comes_From_Source (Hidden)
                       and then Hidden /= Old_S
                     then
                        Error_Msg_Sloc := Sloc (Hidden);
                        Error_Msg_N
                          ("default subprogram is resolved in the generic "
                           & "declaration (RM 12.6(17))??", N);
                        Error_Msg_NE ("\and will not use & #??", N, Hidden);
                     end if;
                  end;
               end if;
            end if;

         else
            Analyze (Nam);

            --  The subprogram renaming declaration may become Ghost if it
            --  renames a Ghost entity.

            if Is_Entity_Name (Nam) then
               Mark_Ghost_Renaming (N, Entity (Nam));
            end if;

            New_S := Analyze_Subprogram_Specification (Spec);
         end if;

      else
         --  Renamed entity must be analyzed first, to avoid being hidden by
         --  new name (which might be the same in a generic instance).

         Analyze (Nam);

         --  The subprogram renaming declaration may become Ghost if it renames
         --  a Ghost entity.

         if Is_Entity_Name (Nam) then
            Mark_Ghost_Renaming (N, Entity (Nam));
         end if;

         --  The renaming defines a new overloaded entity, which is analyzed
         --  like a subprogram declaration.

         New_S := Analyze_Subprogram_Specification (Spec);
      end if;

      if Current_Scope /= Standard_Standard then
         Set_Is_Pure (New_S, Is_Pure (Current_Scope));
      end if;

      --  Set SPARK mode from current context

      Set_SPARK_Pragma           (New_S, SPARK_Mode_Pragma);
      Set_SPARK_Pragma_Inherited (New_S);

      Rename_Spec := Find_Corresponding_Spec (N);

      --  Case of Renaming_As_Body

      if Present (Rename_Spec) then
         Check_Previous_Null_Procedure (N, Rename_Spec);

         --  Renaming declaration is the completion of the declaration of
         --  Rename_Spec. We build an actual body for it at the freezing point.

         Set_Corresponding_Spec (N, Rename_Spec);

         --  Deal with special case of stream functions of abstract types
         --  and interfaces.

         if Nkind (Unit_Declaration_Node (Rename_Spec)) =
                                     N_Abstract_Subprogram_Declaration
         then
            --  Input stream functions are abstract if the object type is
            --  abstract. Similarly, all default stream functions for an
            --  interface type are abstract. However, these subprograms may
            --  receive explicit declarations in representation clauses, making
            --  the attribute subprograms usable as defaults in subsequent
            --  type extensions.
            --  In this case we rewrite the declaration to make the subprogram
            --  non-abstract. We remove the previous declaration, and insert
            --  the new one at the point of the renaming, to prevent premature
            --  access to unfrozen types. The new declaration reuses the
            --  specification of the previous one, and must not be analyzed.

            pragma Assert
              (Is_Primitive (Entity (Nam))
                and then
                  Is_Abstract_Type (Find_Dispatching_Type (Entity (Nam))));
            declare
               Old_Decl : constant Node_Id :=
                            Unit_Declaration_Node (Rename_Spec);
               New_Decl : constant Node_Id :=
                            Make_Subprogram_Declaration (Sloc (N),
                              Specification =>
                                Relocate_Node (Specification (Old_Decl)));
            begin
               Remove (Old_Decl);
               Insert_After (N, New_Decl);
               Set_Is_Abstract_Subprogram (Rename_Spec, False);
               Set_Analyzed (New_Decl);
            end;
         end if;

         Set_Corresponding_Body (Unit_Declaration_Node (Rename_Spec), New_S);

         if Ada_Version = Ada_83 and then Comes_From_Source (N) then
            Error_Msg_N ("(Ada 83) renaming cannot serve as a body", N);
         end if;

         Set_Convention (New_S, Convention (Rename_Spec));
         Check_Fully_Conformant (New_S, Rename_Spec);
         Set_Public_Status (New_S);

         if No_Return (Rename_Spec)
           and then not No_Return (Entity (Nam))
         then
            Error_Msg_NE
              ("renamed subprogram & must be No_Return", N, Entity (Nam));
            Error_Msg_N
              ("\since renaming subprogram is No_Return (RM 6.5.1(7/2))", N);
         end if;

         --  The specification does not introduce new formals, but only
         --  repeats the formals of the original subprogram declaration.
         --  For cross-reference purposes, and for refactoring tools, we
         --  treat the formals of the renaming declaration as body formals.

         Reference_Body_Formals (Rename_Spec, New_S);

         --  Indicate that the entity in the declaration functions like the
         --  corresponding body, and is not a new entity. The body will be
         --  constructed later at the freeze point, so indicate that the
         --  completion has not been seen yet.

         Reinit_Field_To_Zero (New_S, F_Has_Out_Or_In_Out_Parameter,
           Old_Ekind => (E_Function | E_Procedure => True, others => False));
         Reinit_Field_To_Zero (New_S, F_Needs_No_Actuals);
         Reinit_Field_To_Zero (New_S, F_Is_Predicate_Function);
         Reinit_Field_To_Zero (New_S, F_Protected_Subprogram);
         Reinit_Field_To_Zero (New_S, F_Is_Inlined_Always);
         Reinit_Field_To_Zero (New_S, F_Is_Generic_Actual_Subprogram);
         Mutate_Ekind (New_S, E_Subprogram_Body);
         New_S := Rename_Spec;
         Set_Has_Completion (Rename_Spec, False);

         --  Ada 2005: check overriding indicator

         if Present (Overridden_Operation (Rename_Spec)) then
            if Must_Not_Override (Specification (N)) then
               Error_Msg_NE
                 ("subprogram& overrides inherited operation",
                    N, Rename_Spec);

            elsif Style_Check
              and then not Must_Override (Specification (N))
            then
               Style.Missing_Overriding (N, Rename_Spec);
            end if;

         elsif Must_Override (Specification (N))
           and then not Can_Override_Operator (Rename_Spec)
         then
            Error_Msg_NE ("subprogram& is not overriding", N, Rename_Spec);
         end if;

         --  AI12-0132: a renames-as-body freezes the expression of any
         --  expression function that it renames.

         if Is_Entity_Name (Nam)
           and then Is_Expression_Function (Entity (Nam))
           and then not Inside_A_Generic
         then
            Freeze_Expr_Types
              (Def_Id => Entity (Nam),
               Typ    => Etype (Entity (Nam)),
               Expr   =>
                 Expression
                   (Original_Node (Unit_Declaration_Node (Entity (Nam)))),
               N      => N);
         end if;

      --  Normal subprogram renaming (not renaming as body)

      else
         Generate_Definition (New_S);
         New_Overloaded_Entity (New_S);

         if not (Is_Entity_Name (Nam)
                  and then Is_Intrinsic_Subprogram (Entity (Nam)))
         then
            Check_Delayed_Subprogram (New_S);
         end if;

         --  Verify that a SPARK renaming does not declare a primitive
         --  operation of a tagged type.

         Check_SPARK_Primitive_Operation (New_S);
      end if;

      --  There is no need for elaboration checks on the new entity, which may
      --  be called before the next freezing point where the body will appear.
      --  Elaboration checks refer to the real entity, not the one created by
      --  the renaming declaration.

      Set_Kill_Elaboration_Checks (New_S, True);

      --  If we had a previous error, indicate a completion is present to stop
      --  junk cascaded messages, but don't take any further action.

      if Etype (Nam) = Any_Type then
         Set_Has_Completion (New_S);
         return;

      --  Case where name has the form of a selected component

      elsif Nkind (Nam) = N_Selected_Component then

         --  A name which has the form A.B can designate an entry of task A, a
         --  protected operation of protected object A, or finally a primitive
         --  operation of object A. In the later case, A is an object of some
         --  tagged type, or an access type that denotes one such. To further
         --  distinguish these cases, note that the scope of a task entry or
         --  protected operation is type of the prefix.

         --  The prefix could be an overloaded function call that returns both
         --  kinds of operations. This overloading pathology is left to the
         --  dedicated reader ???

         declare
            T : constant Entity_Id := Etype (Prefix (Nam));

         begin
            if Present (T)
              and then
                (Is_Tagged_Type (T)
                  or else
                    (Is_Access_Type (T)
                      and then Is_Tagged_Type (Designated_Type (T))))
              and then Scope (Entity (Selector_Name (Nam))) /= T
            then
               Analyze_Renamed_Primitive_Operation
                 (N, New_S, Present (Rename_Spec));
               return;

            else
               --  Renamed entity is an entry or protected operation. For those
               --  cases an explicit body is built (at the point of freezing of
               --  this entity) that contains a call to the renamed entity.

               --  This is not allowed for renaming as body if the renamed
               --  spec is already frozen (see RM 8.5.4(5) for details).

               if Present (Rename_Spec) and then Is_Frozen (Rename_Spec) then
                  Error_Msg_N
                    ("renaming-as-body cannot rename entry as subprogram", N);
                  Error_Msg_NE
                    ("\since & is already frozen (RM 8.5.4(5))",
                     N, Rename_Spec);
               else
                  Analyze_Renamed_Entry (N, New_S, Present (Rename_Spec));
               end if;

               return;
            end if;
         end;

      --  Case where name is an explicit dereference X.all

      elsif Nkind (Nam) = N_Explicit_Dereference then

         --  Renamed entity is designated by access_to_subprogram expression.
         --  Must build body to encapsulate call, as in the entry case.

         Analyze_Renamed_Dereference (N, New_S, Present (Rename_Spec));
         return;

      --  Indexed component

      elsif Nkind (Nam) = N_Indexed_Component then
         Analyze_Renamed_Family_Member (N, New_S, Present (Rename_Spec));
         return;

      --  Character literal

      elsif Nkind (Nam) = N_Character_Literal then
         Analyze_Renamed_Character (N, New_S, Present (Rename_Spec));
         return;

      --  Only remaining case is where we have a non-entity name, or a renaming
      --  of some other non-overloadable entity.

      elsif not Is_Entity_Name (Nam)
        or else not Is_Overloadable (Entity (Nam))
      then
         --  Do not mention the renaming if it comes from an instance

         if not Is_Actual then
            Error_Msg_N ("expect valid subprogram name in renaming", N);
         else
            Error_Msg_NE ("no visible subprogram for formal&", N, Nam);
         end if;

         return;
      end if;

      --  Find the renamed entity that matches the given specification. Disable
      --  Ada_83 because there is no requirement of full conformance between
      --  renamed entity and new entity, even though the same circuit is used.

      --  This is a bit of an odd case, which introduces a really irregular use
      --  of Ada_Version[_Explicit]. Would be nice to find cleaner way to do
      --  this. ???

      Ada_Version := Ada_Version_Type'Max (Ada_Version, Ada_95);
      Ada_Version_Pragma := Empty;
      Ada_Version_Explicit := Ada_Version;

      if No (Old_S) then
         Old_S := Find_Renamed_Entity (N, Name (N), New_S, Is_Actual);

         --  The visible operation may be an inherited abstract operation that
         --  was overridden in the private part, in which case a call will
         --  dispatch to the overriding operation. Use the overriding one in
         --  the renaming declaration, to prevent spurious errors below.

         if Is_Overloadable (Old_S)
           and then Is_Abstract_Subprogram (Old_S)
           and then No (DTC_Entity (Old_S))
           and then Present (Alias (Old_S))
           and then not Is_Abstract_Subprogram (Alias (Old_S))
           and then Present (Overridden_Operation (Alias (Old_S)))
         then
            Old_S := Alias (Old_S);
         end if;

         --  When the renamed subprogram is overloaded and used as an actual
         --  of a generic, its entity is set to the first available homonym.
         --  We must first disambiguate the name, then set the proper entity.

         if Is_Actual and then Is_Overloaded (Nam) then
            Set_Entity (Nam, Old_S);
         end if;
      end if;

      --  Most common case: subprogram renames subprogram. No body is generated
      --  in this case, so we must indicate the declaration is complete as is.
      --  and inherit various attributes of the renamed subprogram.

      if No (Rename_Spec) then
         Set_Has_Completion   (New_S);
         Set_Is_Imported      (New_S, Is_Imported      (Entity (Nam)));
         Set_Is_Pure          (New_S, Is_Pure          (Entity (Nam)));
         Set_Is_Preelaborated (New_S, Is_Preelaborated (Entity (Nam)));

         --  Ada 2005 (AI-423): Check the consistency of null exclusions
         --  between a subprogram and its correct renaming.

         --  Note: the Any_Id check is a guard that prevents compiler crashes
         --  when performing a null exclusion check between a renaming and a
         --  renamed subprogram that has been found to be illegal.

         if Ada_Version >= Ada_2005 and then Entity (Nam) /= Any_Id then
            Check_Null_Exclusion
              (Ren => New_S,
               Sub => Entity (Nam));
         end if;

         --  Enforce the Ada 2005 rule that the renamed entity cannot require
         --  overriding. The flag Requires_Overriding is set very selectively
         --  and misses some other illegal cases. The additional conditions
         --  checked below are sufficient but not necessary ???

         --  The rule does not apply to the renaming generated for an actual
         --  subprogram in an instance.

         if Is_Actual then
            null;

         --  Guard against previous errors, and omit renamings of predefined
         --  operators.

         elsif Ekind (Old_S) not in E_Function | E_Procedure then
            null;

         elsif Requires_Overriding (Old_S)
           or else
             (Is_Abstract_Subprogram (Old_S)
               and then Present (Find_Dispatching_Type (Old_S))
               and then not Is_Abstract_Type (Find_Dispatching_Type (Old_S)))
         then
            Error_Msg_N
              ("renamed entity cannot be subprogram that requires overriding "
               & "(RM 8.5.4 (5.1))", N);
         end if;

         declare
            Prev : constant Entity_Id := Overridden_Operation (New_S);
         begin
            if Present (Prev)
              and then
                (Has_Non_Trivial_Precondition (Prev)
                  or else Has_Non_Trivial_Precondition (Old_S))
            then
               Error_Msg_NE
                 ("conflicting inherited classwide preconditions in renaming "
                  & "of& (RM 6.1.1 (17)", N, Old_S);
            end if;
         end;
      end if;

      if Old_S /= Any_Id then
         if Is_Actual and then From_Default (N) then

            --  This is an implicit reference to the default actual

            Generate_Reference (Old_S, Nam, Typ => 'i', Force => True);

         else
            Generate_Reference (Old_S, Nam);
         end if;

         Check_Internal_Protected_Use (N, Old_S);

         --  For a renaming-as-body, require subtype conformance, but if the
         --  declaration being completed has not been frozen, then inherit the
         --  convention of the renamed subprogram prior to checking conformance
         --  (unless the renaming has an explicit convention established; the
         --  rule stated in the RM doesn't seem to address this ???).

         if Present (Rename_Spec) then
            Generate_Reference (Rename_Spec, Defining_Entity (Spec), 'b');
            Style.Check_Identifier (Defining_Entity (Spec), Rename_Spec);

            if not Is_Frozen (Rename_Spec) then
               if not Has_Convention_Pragma (Rename_Spec) then
                  Set_Convention (New_S, Convention (Old_S));
               end if;

               if Ekind (Old_S) /= E_Operator then
                  Check_Mode_Conformant (New_S, Old_S, Spec);
               end if;

               if Original_Subprogram (Old_S) = Rename_Spec then
                  Error_Msg_N ("unfrozen subprogram cannot rename itself", N);
               else
                  Check_Formal_Subprogram_Conformance (New_S, Old_S, Spec);
               end if;
            else
               Check_Subtype_Conformant (New_S, Old_S, Spec);
            end if;

            Check_Frozen_Renaming (N, Rename_Spec);

            --  Check explicitly that renamed entity is not intrinsic, because
            --  in a generic the renamed body is not built. In this case,
            --  the renaming_as_body is a completion.

            if Inside_A_Generic then
               if Is_Frozen (Rename_Spec)
                 and then Is_Intrinsic_Subprogram (Old_S)
               then
                  Error_Msg_N
                    ("subprogram in renaming_as_body cannot be intrinsic",
                     Name (N));
               end if;

               Set_Has_Completion (Rename_Spec);
            end if;

         elsif Ekind (Old_S) /= E_Operator then

            --  If this a defaulted subprogram for a class-wide actual there is
            --  no check for mode conformance,  given that the signatures don't
            --  match (the source mentions T but the actual mentions T'Class).

            if CW_Actual then
               null;

            --  No need for a redundant error message if this is a nested
            --  instance, unless the current instantiation (of a child unit)
            --  is a compilation unit, which is not analyzed when the parent
            --  generic is analyzed.

            elsif not Is_Actual
               or else No (Enclosing_Instance)
               or else Is_Compilation_Unit (Current_Scope)
            then
               Check_Mode_Conformant (New_S, Old_S);
            end if;
         end if;

         if No (Rename_Spec) then

            --  The parameter profile of the new entity is that of the renamed
            --  entity: the subtypes given in the specification are irrelevant.

            Inherit_Renamed_Profile (New_S, Old_S);

            --  A call to the subprogram is transformed into a call to the
            --  renamed entity. This is transitive if the renamed entity is
            --  itself a renaming.

            if Present (Alias (Old_S)) then
               Set_Alias (New_S, Alias (Old_S));
            else
               Set_Alias (New_S, Old_S);
            end if;

            --  Note that we do not set Is_Intrinsic_Subprogram if we have a
            --  renaming as body, since the entity in this case is not an
            --  intrinsic (it calls an intrinsic, but we have a real body for
            --  this call, and it is in this body that the required intrinsic
            --  processing will take place).

            --  Also, if this is a renaming of inequality, the renamed operator
            --  is intrinsic, but what matters is the corresponding equality
            --  operator, which may be user-defined.

            Set_Is_Intrinsic_Subprogram
              (New_S,
               Is_Intrinsic_Subprogram (Old_S)
                 and then
                   (Chars (Old_S) /= Name_Op_Ne
                     or else Ekind (Old_S) = E_Operator
                     or else Is_Intrinsic_Subprogram
                               (Corresponding_Equality (Old_S))));

            if Ekind (Alias (New_S)) = E_Operator then
               Set_Has_Delayed_Freeze (New_S, False);
            end if;

            --  If the renaming corresponds to an association for an abstract
            --  formal subprogram, then various attributes must be set to
            --  indicate that the renaming is an abstract dispatching operation
            --  with a controlling type.

            --  Skip this decoration when the renaming corresponds to an
            --  association with class-wide wrapper (see above) because such
            --  wrapper is neither abstract nor a dispatching operation (its
            --  body has the dispatching call to the wrapped primitive).

            if Is_Actual
              and then Is_Abstract_Subprogram (Formal_Spec)
              and then No (Wrapped_Prim)
            then

               --  Mark the renaming as abstract here, so Find_Dispatching_Type
               --  see it as corresponding to a generic association for a
               --  formal abstract subprogram

               Set_Is_Abstract_Subprogram (New_S);

               declare
                  New_S_Ctrl_Type : constant Entity_Id :=
                                      Find_Dispatching_Type (New_S);
                  Old_S_Ctrl_Type : constant Entity_Id :=
                                      Find_Dispatching_Type (Old_S);

               begin

                  --  The actual must match the (instance of the) formal,
                  --  and must be a controlling type.

                  if Old_S_Ctrl_Type /= New_S_Ctrl_Type
                    or else No (New_S_Ctrl_Type)
                  then
                     if No (New_S_Ctrl_Type) then
                        Error_Msg_N
                          ("actual must be dispatching subprogram", Nam);
                     else
                        Error_Msg_NE
                          ("actual must be dispatching subprogram for type&",
                           Nam, New_S_Ctrl_Type);
                     end if;

                  else
                     Set_Is_Dispatching_Operation (New_S);
                     Check_Controlling_Formals (New_S_Ctrl_Type, New_S);

                     --  If the actual in the formal subprogram is itself a
                     --  formal abstract subprogram association, there's no
                     --  dispatch table component or position to inherit.

                     if Present (DTC_Entity (Old_S)) then
                        Set_DTC_Entity  (New_S, DTC_Entity (Old_S));
                        Set_DT_Position_Value (New_S, DT_Position (Old_S));
                     end if;
                  end if;
               end;
            end if;
         end if;

         if Is_Actual then
            null;

         --  The following is illegal, because F hides whatever other F may
         --  be around:
         --     function F (...) renames F;

         elsif Old_S = New_S
           or else (Nkind (Nam) /= N_Expanded_Name
                     and then Chars (Old_S) = Chars (New_S))
         then
            Error_Msg_N ("subprogram cannot rename itself", N);

         --  This is illegal even if we use a selector:
         --     function F (...) renames Pkg.F;
         --  because F is still hidden.

         elsif Nkind (Nam) = N_Expanded_Name
           and then Entity (Prefix (Nam)) = Current_Scope
           and then Chars (Selector_Name (Nam)) = Chars (New_S)
         then
            --  This is an error, but we overlook the error and accept the
            --  renaming if the special Overriding_Renamings mode is in effect.

            if not Overriding_Renamings then
               Error_Msg_NE
                 ("implicit operation& is not visible (RM 8.3 (15))",
                  Nam, Old_S);
            end if;

         --  Check whether an expanded name used for the renamed subprogram
         --  begins with the same name as the renaming itself, and if so,
         --  issue an error about the prefix being hidden by the renaming.
         --  We exclude generic instances from this checking, since such
         --  normally illegal renamings can be constructed when expanding
         --  instantiations.

         elsif Nkind (Nam) = N_Expanded_Name and then not In_Instance then
            declare
               function Ult_Expanded_Prefix (N : Node_Id) return Node_Id is
                 (if Nkind (N) /= N_Expanded_Name
                  then N
                  else Ult_Expanded_Prefix (Prefix (N)));
               --  Returns the ultimate prefix of an expanded name

            begin
               if Chars (Entity (Ult_Expanded_Prefix (Nam))) = Chars (New_S)
               then
                  Error_Msg_Sloc := Sloc (N);
                  Error_Msg_NE
                    ("& is hidden by declaration#", Nam, New_S);
               end if;
            end;
         end if;

         Set_Convention (New_S, Convention (Old_S));

         if Is_Abstract_Subprogram (Old_S) then
            if Present (Rename_Spec) then
               Error_Msg_N
                 ("a renaming-as-body cannot rename an abstract subprogram",
                  N);
               Set_Has_Completion (Rename_Spec);
            else
               Set_Is_Abstract_Subprogram (New_S);
            end if;
         end if;

         Check_Library_Unit_Renaming (N, Old_S);

         --  Pathological case: procedure renames entry in the scope of its
         --  task. Entry is given by simple name, but body must be built for
         --  procedure. Of course if called it will deadlock.

         if Ekind (Old_S) = E_Entry then
            Set_Has_Completion (New_S, False);
            Set_Alias (New_S, Empty);
         end if;

         --  Do not freeze the renaming nor the renamed entity when the context
         --  is an enclosing generic. Freezing is an expansion activity, and in
         --  addition the renamed entity may depend on the generic formals of
         --  the enclosing generic.

         if Is_Actual and not Inside_A_Generic then
            Freeze_Before (N, Old_S);
            Freeze_Actual_Profile;
            Set_Has_Delayed_Freeze (New_S, False);
            Freeze_Before (N, New_S);

            if (Ekind (Old_S) = E_Procedure or else Ekind (Old_S) = E_Function)
              and then not Is_Abstract_Subprogram (Formal_Spec)
            then
               --  An abstract subprogram is only allowed as an actual in the
               --  case where the formal subprogram is also abstract.

               if Is_Abstract_Subprogram (Old_S) then
                  Error_Msg_N
                    ("abstract subprogram not allowed as generic actual", Nam);
               end if;

               --  AI12-0412: A primitive of an abstract type with Pre'Class
               --  or Post'Class aspects specified with nonstatic expressions
               --  is not allowed as actual for a nonabstract formal subprogram
               --  (see RM 6.1.1(18.2/5).

               if Is_Dispatching_Operation (Old_S)
                 and then
                   Is_Prim_Of_Abst_Type_With_Nonstatic_CW_Pre_Post (Old_S)
               then
                  Error_Msg_N
                    ("primitive of abstract type with nonstatic class-wide "
                      & "pre/postconditions not allowed as actual",
                     Nam);
               end if;
            end if;
         end if;

      else
         --  A common error is to assume that implicit operators for types are
         --  defined in Standard, or in the scope of a subtype. In those cases
         --  where the renamed entity is given with an expanded name, it is
         --  worth mentioning that operators for the type are not declared in
         --  the scope given by the prefix.

         if Nkind (Nam) = N_Expanded_Name
           and then Nkind (Selector_Name (Nam)) = N_Operator_Symbol
           and then Scope (Entity (Nam)) = Standard_Standard
         then
            declare
               T : constant Entity_Id :=
                     Base_Type (Etype (First_Formal (New_S)));
            begin
               Error_Msg_Node_2 := Prefix (Nam);
               Error_Msg_NE
                 ("operator for type& is not declared in&", Prefix (Nam), T);
            end;

         else
            Error_Msg_NE
              ("no visible subprogram matches the specification for&",
                Spec, New_S);
         end if;

         if Present (Candidate_Renaming) then
            declare
               F1 : Entity_Id;
               F2 : Entity_Id;
               T1 : Entity_Id;

            begin
               F1 := First_Formal (Candidate_Renaming);
               F2 := First_Formal (New_S);
               T1 := First_Subtype (Etype (F1));
               while Present (F1) and then Present (F2) loop
                  Next_Formal (F1);
                  Next_Formal (F2);
               end loop;

               if Present (F1) and then Present (Default_Value (F1)) then
                  if Present (Next_Formal (F1)) then
                     Error_Msg_NE
                       ("\missing specification for & and other formals with "
                        & "defaults", Spec, F1);
                  else
                     Error_Msg_NE ("\missing specification for &", Spec, F1);
                  end if;
               end if;

               if Nkind (Nam) = N_Operator_Symbol
                 and then From_Default (N)
               then
                  Error_Msg_Node_2 := T1;
                  Error_Msg_NE
                    ("default & on & is not directly visible", Nam, Nam);
               end if;
            end;
         end if;
      end if;

      --  Ada 2005 AI 404: if the new subprogram is dispatching, verify that
      --  controlling access parameters are known non-null for the renamed
      --  subprogram. Test also applies to a subprogram instantiation that
      --  is dispatching. Test is skipped if some previous error was detected
      --  that set Old_S to Any_Id.

      if Ada_Version >= Ada_2005
        and then Old_S /= Any_Id
        and then not Is_Dispatching_Operation (Old_S)
        and then Is_Dispatching_Operation (New_S)
      then
         declare
            Old_F : Entity_Id;
            New_F : Entity_Id;

         begin
            Old_F := First_Formal (Old_S);
            New_F := First_Formal (New_S);
            while Present (Old_F) loop
               if Ekind (Etype (Old_F)) = E_Anonymous_Access_Type
                 and then Is_Controlling_Formal (New_F)
                 and then not Can_Never_Be_Null (Old_F)
               then
                  Error_Msg_N ("access parameter is controlling,", New_F);
                  Error_Msg_NE
                    ("\corresponding parameter of& must be explicitly null "
                     & "excluding", New_F, Old_S);
               end if;

               Next_Formal (Old_F);
               Next_Formal (New_F);
            end loop;
         end;
      end if;

      --  A useful warning, suggested by Ada Bug Finder (Ada-Europe 2005)
      --  is to warn if an operator is being renamed as a different operator.
      --  If the operator is predefined, examine the kind of the entity, not
      --  the abbreviated declaration in Standard.

      if Comes_From_Source (N)
        and then Present (Old_S)
        and then (Nkind (Old_S) = N_Defining_Operator_Symbol
                   or else Ekind (Old_S) = E_Operator)
        and then Nkind (New_S) = N_Defining_Operator_Symbol
        and then Chars (Old_S) /= Chars (New_S)
      then
         Error_Msg_NE
           ("& is being renamed as a different operator??", N, Old_S);
      end if;

      --  Check for renaming of obsolescent subprogram

      Check_Obsolescent_2005_Entity (Entity (Nam), Nam);

      --  Another warning or some utility: if the new subprogram as the same
      --  name as the old one, the old one is not hidden by an outer homograph,
      --  the new one is not a public symbol, and the old one is otherwise
      --  directly visible, the renaming is superfluous.

      if Chars (Old_S) = Chars (New_S)
        and then Comes_From_Source (N)
        and then Scope (Old_S) /= Standard_Standard
        and then Warn_On_Redundant_Constructs
        and then (Is_Immediately_Visible (Old_S)
                   or else Is_Potentially_Use_Visible (Old_S))
        and then Is_Overloadable (Current_Scope)
        and then Chars (Current_Scope) /= Chars (Old_S)
      then
         Error_Msg_N
           ("redundant renaming, entity is directly visible?r?", Name (N));
      end if;

      --  Implementation-defined aspect specifications can appear in a renaming
      --  declaration, but not language-defined ones. The call to procedure
      --  Analyze_Aspect_Specifications will take care of this error check.

      Analyze_Aspect_Specifications (N, New_S);

      --  AI12-0279

      if Is_Actual
        and then Has_Yield_Aspect (Formal_Spec)
        and then not Has_Yield_Aspect (Old_S)
      then
         Error_Msg_Name_1 := Name_Yield;
         Error_Msg_N
           ("actual subprogram& must have aspect% to match formal", Name (N));
      end if;

      Ada_Version := Save_AV;
      Ada_Version_Pragma := Save_AVP;
      Ada_Version_Explicit := Save_AV_Exp;

      --  Check if we are looking at an Ada 2012 defaulted formal subprogram
      --  and mark any use_package_clauses that affect the visibility of the
      --  implicit generic actual.

      --  Also, we may be looking at an internal renaming of a user-defined
      --  subprogram created for a generic formal subprogram association,
      --  which will also have to be marked here. This can occur when the
      --  corresponding formal subprogram contains references to other generic
      --  formals.

      if Is_Generic_Actual_Subprogram (New_S)
        and then (Is_Intrinsic_Subprogram (New_S)
                   or else From_Default (N)
                   or else Nkind (N) = N_Subprogram_Renaming_Declaration)
      then
         Mark_Use_Clauses (New_S);

         --  Handle overloaded subprograms

         if Present (Alias (New_S)) then
            Mark_Use_Clauses (Alias (New_S));
         end if;
      end if;

      if Is_Actual then
         Local_Restrict.Check_Actual_Subprogram_For_Instance
           (Actual_Subp_Name => Nam, Formal_Subp => Formal_Spec);
      end if;
   end Analyze_Subprogram_Renaming;

   -------------------------
   -- Analyze_Use_Package --
   -------------------------

   --  Resolve the package names in the use clause, and make all the visible
   --  entities defined in the package potentially use-visible. If the package
   --  is already in use from a previous use clause, its visible entities are
   --  already use-visible. In that case, mark the occurrence as a redundant
   --  use. If the package is an open scope, i.e. if the use clause occurs
   --  within the package itself, ignore it.

   procedure Analyze_Use_Package (N : Node_Id; Chain : Boolean := True) is
      procedure Analyze_Package_Name (Clause : Node_Id);
      --  Perform analysis on a package name from a use_package_clause

      procedure Analyze_Package_Name_List (Head_Clause : Node_Id);
      --  Similar to Analyze_Package_Name but iterates over all the names
      --  in a use clause.

      --------------------------
      -- Analyze_Package_Name --
      --------------------------

      procedure Analyze_Package_Name (Clause : Node_Id) is
         Pack : constant Node_Id := Name (Clause);
         Pref : Node_Id;

      begin
         pragma Assert (Nkind (Clause) = N_Use_Package_Clause);
         Analyze (Pack);

         --  Verify that the package standard is not directly named in a
         --  use_package_clause.

         if Nkind (Parent (Clause)) = N_Compilation_Unit
           and then Nkind (Pack) = N_Expanded_Name
         then
            Pref := Prefix (Pack);

            while Nkind (Pref) = N_Expanded_Name loop
               Pref := Prefix (Pref);
            end loop;

            if Entity (Pref) = Standard_Standard then
               Error_Msg_N
                 ("predefined package Standard cannot appear in a context "
                  & "clause", Pref);
            end if;
         end if;
      end Analyze_Package_Name;

      -------------------------------
      -- Analyze_Package_Name_List --
      -------------------------------

      procedure Analyze_Package_Name_List (Head_Clause : Node_Id) is
         Curr : Node_Id;

      begin
         --  Due to the way source use clauses are split during parsing we are
         --  forced to simply iterate through all entities in scope until the
         --  clause representing the last name in the list is found.

         Curr := Head_Clause;
         while Present (Curr) loop
            Analyze_Package_Name (Curr);

            --  Stop iterating over the names in the use clause when we are at
            --  the last one.

            exit when not More_Ids (Curr) and then Prev_Ids (Curr);
            Next (Curr);
         end loop;
      end Analyze_Package_Name_List;

      --  Local variables

      Pack : Entity_Id;

   --  Start of processing for Analyze_Use_Package

   begin
      Set_Hidden_By_Use_Clause (N, No_Elist);

      --  Use clause not allowed in a spec of a predefined package declaration
      --  except that packages whose file name starts a-n are OK (these are
      --  children of Ada.Numerics, which are never loaded by Rtsfind).

      if Is_Predefined_Unit (Current_Sem_Unit)
        and then Get_Name_String
                   (Unit_File_Name (Current_Sem_Unit)) (1 .. 3) /= "a-n"
        and then Nkind (Unit (Cunit (Current_Sem_Unit))) =
                   N_Package_Declaration
      then
         Error_Msg_N ("use clause not allowed in predefined spec", N);
      end if;

      --  Loop through all package names from the original use clause in
      --  order to analyze referenced packages. A use_package_clause with only
      --  one name does not have More_Ids or Prev_Ids set, while a clause with
      --  More_Ids only starts the chain produced by the parser.

      if not More_Ids (N) and then not Prev_Ids (N) then
         Analyze_Package_Name (N);

      elsif More_Ids (N) and then not Prev_Ids (N) then
         Analyze_Package_Name_List (N);
      end if;

      if not Is_Entity_Name (Name (N)) then
         Error_Msg_N ("& is not a package", Name (N));

         return;
      end if;

      if Chain then
         Chain_Use_Clause (N);
      end if;

      Pack := Entity (Name (N));

      --  There are many cases where scopes are manipulated during analysis, so
      --  check that Pack's current use clause has not already been chained
      --  before setting its previous use clause.

      if Ekind (Pack) = E_Package
        and then Present (Current_Use_Clause (Pack))
        and then Current_Use_Clause (Pack) /= N
        and then No (Prev_Use_Clause (N))
        and then Prev_Use_Clause (Current_Use_Clause (Pack)) /= N
      then
         Set_Prev_Use_Clause (N, Current_Use_Clause (Pack));
      end if;

      --  Mark all entities as potentially use visible

      if Ekind (Pack) /= E_Package and then Etype (Pack) /= Any_Type then
         if Ekind (Pack) = E_Generic_Package then
            Error_Msg_N  -- CODEFIX
              ("a generic package is not allowed in a use clause", Name (N));

         elsif Is_Generic_Subprogram (Pack) then
            Error_Msg_N  -- CODEFIX
              ("a generic subprogram is not allowed in a use clause",
               Name (N));

         elsif Is_Subprogram (Pack) then
            Error_Msg_N  -- CODEFIX
              ("a subprogram is not allowed in a use clause", Name (N));

         else
            Error_Msg_N ("& is not allowed in a use clause", Name (N));
         end if;

      else
         if Nkind (Parent (N)) = N_Compilation_Unit then
            Check_In_Previous_With_Clause (N, Name (N));
         end if;

         Use_One_Package (N, Name (N));
      end if;

      Mark_Ghost_Clause (N);
   end Analyze_Use_Package;

   ----------------------
   -- Analyze_Use_Type --
   ----------------------

   procedure Analyze_Use_Type (N : Node_Id; Chain : Boolean := True) is
      E  : Entity_Id;
      Id : Node_Id;

   begin
      Set_Hidden_By_Use_Clause (N, No_Elist);

      --  Chain clause to list of use clauses in current scope when flagged

      if Chain then
         Chain_Use_Clause (N);
      end if;

      --  Obtain the base type of the type denoted within the use_type_clause's
      --  subtype mark.

      Id := Subtype_Mark (N);
      Find_Type (Id);
      E := Base_Type (Entity (Id));

      --  There are many cases where a use_type_clause may be reanalyzed due to
      --  manipulation of the scope stack so we much guard against those cases
      --  here, otherwise, we must add the new use_type_clause to the previous
      --  use_type_clause chain in order to mark redundant use_type_clauses as
      --  used. When the redundant use-type clauses appear in a parent unit and
      --  a child unit we must prevent a circularity in the chain that would
      --  otherwise result from the separate steps of analysis and installation
      --  of the parent context.

      if Present (Current_Use_Clause (E))
        and then Current_Use_Clause (E) /= N
        and then Prev_Use_Clause (Current_Use_Clause (E)) /= N
        and then No (Prev_Use_Clause (N))
      then
         Set_Prev_Use_Clause (N, Current_Use_Clause (E));
      end if;

      --  If the Used_Operations list is already initialized, the clause has
      --  been analyzed previously, and it is being reinstalled, for example
      --  when the clause appears in a package spec and we are compiling the
      --  corresponding package body. In that case, make the entities on the
      --  existing list use_visible, and mark the corresponding types In_Use.

      if Present (Used_Operations (N)) then
         declare
            Elmt : Elmt_Id;

         begin
            Use_One_Type (Subtype_Mark (N), Installed => True);

            Elmt := First_Elmt (Used_Operations (N));
            while Present (Elmt) loop
               Set_Is_Potentially_Use_Visible (Node (Elmt));
               Next_Elmt (Elmt);
            end loop;
         end;

         return;
      end if;

      --  Otherwise, create new list and attach to it the operations that are
      --  made use-visible by the clause.

      Set_Used_Operations (N, New_Elmt_List);
      E := Entity (Id);

      if E /= Any_Type then
         Use_One_Type (Id);

         if Nkind (Parent (N)) = N_Compilation_Unit then
            if Nkind (Id) = N_Identifier then
               Error_Msg_N ("type is not directly visible", Id);

            elsif Is_Child_Unit (Scope (E))
              and then Scope (E) /= System_Aux_Id
            then
               Check_In_Previous_With_Clause (N, Prefix (Id));
            end if;
         end if;

      else
         --  If the use_type_clause appears in a compilation unit context,
         --  check whether it comes from a unit that may appear in a
         --  limited_with_clause, for a better error message.

         if Nkind (Parent (N)) = N_Compilation_Unit
           and then Nkind (Id) /= N_Identifier
         then
            declare
               Item : Node_Id;
               Pref : Node_Id;

               function Mentioned (Nam : Node_Id) return Boolean;
               --  Check whether the prefix of expanded name for the type
               --  appears in the prefix of some limited_with_clause.

               ---------------
               -- Mentioned --
               ---------------

               function Mentioned (Nam : Node_Id) return Boolean is
               begin
                  return Nkind (Name (Item)) = N_Selected_Component
                    and then Chars (Prefix (Name (Item))) = Chars (Nam);
               end Mentioned;

            begin
               Pref := Prefix (Id);
               Item := First (Context_Items (Parent (N)));
               while Present (Item) and then Item /= N loop
                  if Nkind (Item) = N_With_Clause
                    and then Limited_Present (Item)
                    and then Mentioned (Pref)
                  then
                     Change_Error_Text
                       (Get_Msg_Id, "premature usage of incomplete type");
                  end if;

                  Next (Item);
               end loop;
            end;
         end if;
      end if;

      Mark_Ghost_Clause (N);
   end Analyze_Use_Type;

   ------------------------
   -- Attribute_Renaming --
   ------------------------

   procedure Attribute_Renaming (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Nam   : constant Node_Id    := Name (N);
      Spec  : constant Node_Id    := Specification (N);
      New_S : constant Entity_Id  := Defining_Unit_Name (Spec);
      Aname : constant Name_Id    := Attribute_Name (Nam);

      Form_Num  : Nat      := 0;
      Expr_List : List_Id  := No_List;

      Attr_Node  : Node_Id;
      Body_Node  : Node_Id;
      Param_Spec : Node_Id;

   begin
      Generate_Definition (New_S);

      --  This procedure is called in the context of subprogram renaming, and
      --  thus the attribute must be one that is a subprogram. All of those
      --  have at least one formal parameter, with the exceptions of the GNAT
      --  attribute 'Img, which GNAT treats as renameable.

      if Is_Empty_List (Parameter_Specifications (Spec)) then
         if Aname /= Name_Img then
            Error_Msg_N
              ("subprogram renaming an attribute must have formals", N);
            return;
         end if;

      else
         Param_Spec := First (Parameter_Specifications (Spec));
         while Present (Param_Spec) loop
            Form_Num := Form_Num + 1;

            if Nkind (Parameter_Type (Param_Spec)) /= N_Access_Definition then
               Find_Type (Parameter_Type (Param_Spec));

               --  The profile of the new entity denotes the base type (s) of
               --  the types given in the specification. For access parameters
               --  there are no subtypes involved.

               Rewrite (Parameter_Type (Param_Spec),
                 New_Occurrence_Of
                   (Base_Type (Entity (Parameter_Type (Param_Spec))), Loc));
            end if;

            if No (Expr_List) then
               Expr_List := New_List;
            end if;

            Append_To (Expr_List,
              Make_Identifier (Loc,
                Chars => Chars (Defining_Identifier (Param_Spec))));

            --  The expressions in the attribute reference are not freeze
            --  points. Neither is the attribute as a whole, see below.

            Set_Must_Not_Freeze (Last (Expr_List));
            Next (Param_Spec);
         end loop;
      end if;

      --  Immediate error if too many formals. Other mismatches in number or
      --  types of parameters are detected when we analyze the body of the
      --  subprogram that we construct.

      if Form_Num > 2 then
         Error_Msg_N ("too many formals for attribute", N);

      --  Error if the attribute reference has expressions that look like
      --  formal parameters.

      elsif Present (Expressions (Nam)) then
         Error_Msg_N ("illegal expressions in attribute reference", Nam);

      elsif Aname in Name_Compose | Name_Exponent | Name_Leading_Part |
                     Name_Pos     | Name_Round    | Name_Scaling      |
                     Name_Val
      then
         if Nkind (N) = N_Subprogram_Renaming_Declaration
           and then Present (Corresponding_Formal_Spec (N))
         then
            Error_Msg_N
              ("generic actual cannot be attribute involving universal type",
               Nam);
         else
            Error_Msg_N
              ("attribute involving a universal type cannot be renamed",
               Nam);
         end if;
      end if;

      --  Rewrite attribute node to have a list of expressions corresponding to
      --  the subprogram formals. A renaming declaration is not a freeze point,
      --  and the analysis of the attribute reference should not freeze the
      --  type of the prefix. We use the original node in the renaming so that
      --  its source location is preserved, and checks on stream attributes are
      --  properly applied.

      Attr_Node := Relocate_Node (Nam);
      Set_Expressions (Attr_Node, Expr_List);

      Set_Must_Not_Freeze (Attr_Node);
      Set_Must_Not_Freeze (Prefix (Nam));

      --  Case of renaming a function

      if Nkind (Spec) = N_Function_Specification then
         if Is_Procedure_Attribute_Name (Aname) then
            Error_Msg_N ("attribute can only be renamed as procedure", Nam);
            return;
         end if;

         Find_Type (Result_Definition (Spec));
         Rewrite (Result_Definition (Spec),
           New_Occurrence_Of
             (Base_Type (Entity (Result_Definition (Spec))), Loc));

         Body_Node :=
           Make_Subprogram_Body (Loc,
             Specification => Spec,
             Declarations => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                   Statements => New_List (
                     Make_Simple_Return_Statement (Loc,
                       Expression => Attr_Node))));

      --  Case of renaming a procedure

      else
         if not Is_Procedure_Attribute_Name (Aname) then
            Error_Msg_N ("attribute can only be renamed as function", Nam);
            return;
         end if;

         Body_Node :=
           Make_Subprogram_Body (Loc,
             Specification => Spec,
             Declarations => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                   Statements => New_List (Attr_Node)));
      end if;

      --  Signal the ABE mechanism that the generated subprogram body has not
      --  ABE ramifications.

      Set_Was_Attribute_Reference (Body_Node);

      --  In case of tagged types we add the body of the generated function to
      --  the freezing actions of the type (because in the general case such
      --  type is still not frozen). We exclude from this processing generic
      --  formal subprograms found in instantiations.

      --  We must exclude restricted run-time libraries because
      --  entity AST_Handler is defined in package System.Aux_Dec which is not
      --  available in those platforms. Note that we cannot use the function
      --  Restricted_Profile (instead of Configurable_Run_Time_Mode) because
      --  the ZFP run-time library is not defined as a profile, and we do not
      --  want to deal with AST_Handler in ZFP mode.

      if not Configurable_Run_Time_Mode
        and then No (Corresponding_Formal_Spec (N))
        and then not Is_RTE (Etype (Nam), RE_AST_Handler)
      then
         declare
            P : constant Node_Id := Prefix (Nam);

         begin
            --  The prefix of 'Img is an object that is evaluated for each call
            --  of the function that renames it.

            if Aname = Name_Img then
               Preanalyze_And_Resolve (P);

            --  For all other attribute renamings, the prefix is a subtype

            else
               Find_Type (P);
            end if;

            --  If the target type is not yet frozen, add the body to the
            --  actions to be elaborated at freeze time.

            if Is_Tagged_Type (Etype (P))
              and then In_Open_Scopes (Scope (Etype (P)))
            then
               Append_Freeze_Action (Etype (P), Body_Node);
            else
               Rewrite (N, Body_Node);
               Analyze (N);
               Set_Etype (New_S, Base_Type (Etype (New_S)));
            end if;
         end;

      --  Generic formal subprograms or AST_Handler renaming

      else
         Rewrite (N, Body_Node);
         Analyze (N);
         Set_Etype (New_S, Base_Type (Etype (New_S)));
      end if;

      if Is_Compilation_Unit (New_S) then
         Error_Msg_N
           ("a library unit can only rename another library unit", N);
      end if;

      --  We suppress elaboration warnings for the resulting entity, since
      --  clearly they are not needed, and more particularly, in the case
      --  of a generic formal subprogram, the resulting entity can appear
      --  after the instantiation itself, and thus look like a bogus case
      --  of access before elaboration.

      if Legacy_Elaboration_Checks then
         Set_Suppress_Elaboration_Warnings (New_S);
      end if;
   end Attribute_Renaming;

   ----------------------
   -- Chain_Use_Clause --
   ----------------------

   procedure Chain_Use_Clause (N : Node_Id) is
      Level : Int := Scope_Stack.Last;
      Pack  : Entity_Id;

   begin
      --  Common case

      if not Is_Compilation_Unit (Current_Scope)
        or else not Is_Child_Unit (Current_Scope)
      then
         null;

      --  Common case for compilation unit

      elsif Defining_Entity (Parent (N)) = Current_Scope then
         null;

      else
         --  If declaration appears in some other scope, it must be in some
         --  parent unit when compiling a child.

         Pack := Defining_Entity (Parent (N));

         if not In_Open_Scopes (Pack) then
            null;

         --  If the use clause appears in an ancestor and we are in the
         --  private part of the immediate parent, the use clauses are
         --  already installed.

         elsif Pack /= Scope (Current_Scope)
           and then In_Private_Part (Scope (Current_Scope))
         then
            null;

         else
            --  Find entry for parent unit in scope stack

            while Scope_Stack.Table (Level).Entity /= Pack loop
               Level := Level - 1;
            end loop;
         end if;
      end if;

      Set_Next_Use_Clause (N,
        Scope_Stack.Table (Level).First_Use_Clause);
      Scope_Stack.Table (Level).First_Use_Clause := N;
   end Chain_Use_Clause;

   ---------------------------
   -- Check_Frozen_Renaming --
   ---------------------------

   procedure Check_Frozen_Renaming (N : Node_Id; Subp : Entity_Id) is
      B_Node : Node_Id;
      Old_S  : Entity_Id;

   begin
      if Is_Frozen (Subp) and then not Has_Completion (Subp) then
         B_Node :=
           Build_Renamed_Body
             (Parent (Declaration_Node (Subp)), Defining_Entity (N));

         if Is_Entity_Name (Name (N)) then
            Old_S := Entity (Name (N));

            if not Is_Frozen (Old_S)
              and then Operating_Mode /= Check_Semantics
            then
               Append_Freeze_Action (Old_S, B_Node);
            else
               Insert_After (N, B_Node);
               Analyze (B_Node);
            end if;

            if Is_Intrinsic_Subprogram (Old_S)
              and then not In_Instance
              and then not Relaxed_RM_Semantics
            then
               Error_Msg_N
                 ("subprogram used in renaming_as_body cannot be intrinsic",
                  Name (N));
            end if;

         else
            Insert_After (N, B_Node);
            Analyze (B_Node);
         end if;
      end if;
   end Check_Frozen_Renaming;

   -------------------------------
   -- Set_Entity_Or_Discriminal --
   -------------------------------

   procedure Set_Entity_Or_Discriminal (N : Node_Id; E : Entity_Id) is
      P : Node_Id;

   begin
      --  If the entity is not a discriminant, or else expansion is disabled,
      --  simply set the entity.

      if not In_Spec_Expression
        or else Ekind (E) /= E_Discriminant
        or else Inside_A_Generic
      then
         Set_Entity_With_Checks (N, E);

      --  The replacement of a discriminant by the corresponding discriminal
      --  is not done for a task discriminant that appears in a default
      --  expression of an entry parameter. See Exp_Ch2.Expand_Discriminant
      --  for details on their handling.

      elsif Is_Concurrent_Type (Scope (E)) then
         P := Parent (N);
         while Present (P)
           and then Nkind (P) not in
             N_Parameter_Specification | N_Component_Declaration
         loop
            P := Parent (P);
         end loop;

         if Present (P)
           and then Nkind (P) = N_Parameter_Specification
         then
            null;

         --  Don't replace a non-qualified discriminant in preanalysis mode
         --  since we may be preanalyzing a subtree that it is still not
         --  placed in its final position in the tree, and therefore it can
         --  lead to errors during full analysis when the discriminant gets
         --  referenced later. We exclude preanalysis of pragma expressions
         --  because they will not be moved.

         --  This can occur in situations where a protected type contains
         --  an expression function which references a non-prefixed
         --  discriminant, or a task type sets the size of its secondary
         --  stack with a value provided in a discriminant.

         elsif No (P)
           and then Preanalysis_Active
           and then not In_Pragma_Expression (N)
         then
            null;

         else
            Set_Entity (N, Discriminal (E));
         end if;

         --  Otherwise, this is a discriminant in a context in which
         --  it is a reference to the corresponding parameter of the
         --  init proc for the enclosing type.

      else
         Set_Entity (N, Discriminal (E));
      end if;
   end Set_Entity_Or_Discriminal;

   -----------------------------------
   -- Check_In_Previous_With_Clause --
   -----------------------------------

   procedure Check_In_Previous_With_Clause (N, Nam : Node_Id) is
      Pack : constant Entity_Id := Entity (Original_Node (Nam));
      Item : Node_Id;
      Par  : Node_Id;

   begin
      Item := First (Context_Items (Parent (N)));
      while Present (Item) and then Item /= N loop
         if Nkind (Item) = N_With_Clause

           --  Protect the frontend against previous critical errors

           and then Nkind (Name (Item)) /= N_Selected_Component
           and then Entity (Name (Item)) = Pack
         then
            Par := Nam;

            --  Find root library unit in with_clause

            while Nkind (Par) = N_Expanded_Name loop
               Par := Prefix (Par);
            end loop;

            if Is_Child_Unit (Entity (Original_Node (Par))) then
               Error_Msg_NE ("& is not directly visible", Par, Entity (Par));
            else
               return;
            end if;
         end if;

         Next (Item);
      end loop;

      --  On exit, package is not mentioned in a previous with_clause.
      --  Check if its prefix is.

      if Nkind (Nam) = N_Expanded_Name then
         Check_In_Previous_With_Clause (N, Prefix (Nam));

      elsif Pack /= Any_Id then
         Error_Msg_NE ("& is not visible", Nam, Pack);
      end if;
   end Check_In_Previous_With_Clause;

   ---------------------------------
   -- Check_Library_Unit_Renaming --
   ---------------------------------

   procedure Check_Library_Unit_Renaming (N : Node_Id; Old_E : Entity_Id) is
      New_E : Entity_Id;

   begin
      if Nkind (Parent (N)) /= N_Compilation_Unit then
         return;

      --  Check for library unit. Note that we used to check for the scope
      --  being Standard here, but that was wrong for Standard itself.

      elsif not Is_Compilation_Unit (Old_E)
        and then not Is_Child_Unit (Old_E)
      then
         Error_Msg_N ("renamed unit must be a library unit", Name (N));

      --  Entities defined in Standard (operators and boolean literals) cannot
      --  be renamed as library units.

      elsif Scope (Old_E) = Standard_Standard
        and then Sloc (Old_E) = Standard_Location
      then
         Error_Msg_N ("renamed unit must be a library unit", Name (N));

      elsif Present (Parent_Spec (N))
        and then Nkind (Unit (Parent_Spec (N))) = N_Generic_Package_Declaration
        and then not Is_Child_Unit (Old_E)
      then
         Error_Msg_N
           ("renamed unit must be a child unit of generic parent", Name (N));

      elsif Nkind (N) in N_Generic_Renaming_Declaration
        and then  Nkind (Name (N)) = N_Expanded_Name
        and then Is_Generic_Instance (Entity (Prefix (Name (N))))
        and then Is_Generic_Unit (Old_E)
      then
         Error_Msg_N
           ("renamed generic unit must be a library unit", Name (N));

      elsif Is_Package_Or_Generic_Package (Old_E) then

         --  Inherit categorization flags

         New_E := Defining_Entity (N);
         Set_Is_Pure                  (New_E, Is_Pure           (Old_E));
         Set_Is_Preelaborated         (New_E, Is_Preelaborated  (Old_E));
         Set_Is_Remote_Call_Interface (New_E,
                                       Is_Remote_Call_Interface (Old_E));
         Set_Is_Remote_Types          (New_E, Is_Remote_Types   (Old_E));
         Set_Is_Shared_Passive        (New_E, Is_Shared_Passive (Old_E));
      end if;
   end Check_Library_Unit_Renaming;

   ------------------------
   -- Enclosing_Instance --
   ------------------------

   function Enclosing_Instance return Entity_Id is
      S : Entity_Id;

   begin
      if not Is_Generic_Instance (Current_Scope) then
         return Empty;
      end if;

      S := Scope (Current_Scope);
      while S /= Standard_Standard loop
         if Is_Generic_Instance (S) then
            return S;
         end if;

         S := Scope (S);
      end loop;

      return Empty;
   end Enclosing_Instance;

   ---------------
   -- End_Scope --
   ---------------

   procedure End_Scope is
      Id    : Entity_Id;
      Prev  : Entity_Id;
      Outer : Entity_Id;

   begin
      Id := First_Entity (Current_Scope);
      while Present (Id) loop
         --  An entity in the current scope is not necessarily the first one
         --  on its homonym chain. Find its predecessor if any,
         --  If it is an internal entity, it will not be in the visibility
         --  chain altogether,  and there is nothing to unchain.

         if Id /= Current_Entity (Id) then
            Prev := Current_Entity (Id);
            while Present (Prev)
              and then Homonym (Prev) /= Id
            loop
               Prev := Homonym (Prev);
            end loop;

            --  Skip to end of loop if Id is not in the visibility chain

            if No (Prev) then
               goto Next_Ent;
            end if;

         else
            Prev := Empty;
         end if;

         Set_Is_Immediately_Visible (Id, False);

         Outer := Homonym (Id);
         while Present (Outer) and then Scope (Outer) = Current_Scope loop
            Outer := Homonym (Outer);
         end loop;

         --  Reset homonym link of other entities, but do not modify link
         --  between entities in current scope, so that the back-end can have
         --  a proper count of local overloadings.

         if No (Prev) then
            Set_Name_Entity_Id (Chars (Id), Outer);

         elsif Scope (Prev) /= Scope (Id) then
            Set_Homonym (Prev,  Outer);
         end if;

         <<Next_Ent>>
            Next_Entity (Id);
      end loop;

      --  If the scope generated freeze actions, place them before the
      --  current declaration and analyze them. Type declarations and
      --  the bodies of initialization procedures can generate such nodes.
      --  We follow the parent chain until we reach a list node, which is
      --  the enclosing list of declarations. If the list appears within
      --  a protected definition, move freeze nodes outside the protected
      --  type altogether.

      if Present
         (Scope_Stack.Table (Scope_Stack.Last).Pending_Freeze_Actions)
      then
         declare
            Decl : Node_Id;
            L    : constant List_Id := Scope_Stack.Table
                    (Scope_Stack.Last).Pending_Freeze_Actions;

         begin
            if Is_Itype (Current_Scope) then
               Decl := Associated_Node_For_Itype (Current_Scope);
            else
               Decl := Parent (Current_Scope);
            end if;

            Pop_Scope;

            while not Is_List_Member (Decl)
              or else Nkind (Parent (Decl)) in N_Protected_Definition
                                             | N_Task_Definition
            loop
               Decl := Parent (Decl);
            end loop;

            Insert_List_Before_And_Analyze (Decl, L);
         end;

      else
         Pop_Scope;
      end if;
   end End_Scope;

   ---------------------
   -- End_Use_Clauses --
   ---------------------

   procedure End_Use_Clauses (Clause : Node_Id) is
      U : Node_Id;

   begin
      --  Remove use_type_clauses first, because they affect the visibility of
      --  operators in subsequent used packages.

      U := Clause;
      while Present (U) loop
         if Nkind (U) = N_Use_Type_Clause then
            End_Use_Type (U);
         end if;

         Next_Use_Clause (U);
      end loop;

      U := Clause;
      while Present (U) loop
         if Nkind (U) = N_Use_Package_Clause then
            End_Use_Package (U);
         end if;

         Next_Use_Clause (U);
      end loop;
   end End_Use_Clauses;

   ---------------------
   -- End_Use_Package --
   ---------------------

   procedure End_Use_Package (N : Node_Id) is
      Pack      : Entity_Id;
      Pack_Name : Node_Id;
      Id        : Entity_Id;
      Elmt      : Elmt_Id;

      function Is_Primitive_Operator_In_Use
        (Op : Entity_Id;
         F  : Entity_Id) return Boolean;
      --  Check whether Op is a primitive operator of a use-visible type

      ----------------------------------
      -- Is_Primitive_Operator_In_Use --
      ----------------------------------

      function Is_Primitive_Operator_In_Use
        (Op : Entity_Id;
         F  : Entity_Id) return Boolean
      is
         T : constant Entity_Id := Base_Type (Etype (F));
      begin
         return In_Use (T) and then Scope (T) = Scope (Op);
      end Is_Primitive_Operator_In_Use;

   --  Start of processing for End_Use_Package

   begin
      Pack_Name := Name (N);

      --  Test that Pack_Name actually denotes a package before processing

      if Is_Entity_Name (Pack_Name)
        and then Ekind (Entity (Pack_Name)) = E_Package
      then
         Pack := Entity (Pack_Name);

         if In_Open_Scopes (Pack) then
            null;

         elsif not Redundant_Use (Pack_Name) then
            Set_In_Use (Pack, False);
            Set_Current_Use_Clause (Pack, Empty);

            Id := First_Entity (Pack);
            while Present (Id) loop

               --  Preserve use-visibility of operators that are primitive
               --  operators of a type that is use-visible through an active
               --  use_type_clause.

               if Nkind (Id) = N_Defining_Operator_Symbol
                 and then
                   (Is_Primitive_Operator_In_Use (Id, First_Formal (Id))
                     or else
                       (Present (Next_Formal (First_Formal (Id)))
                         and then
                           Is_Primitive_Operator_In_Use
                             (Id, Next_Formal (First_Formal (Id)))))
               then
                  null;
               else
                  Set_Is_Potentially_Use_Visible (Id, False);
               end if;

               if Is_Private_Type (Id)
                 and then Present (Full_View (Id))
               then
                  Set_Is_Potentially_Use_Visible (Full_View (Id), False);
               end if;

               Next_Entity (Id);
            end loop;

            if Present (Renamed_Entity (Pack)) then
               Set_In_Use (Renamed_Entity (Pack), False);
               Set_Current_Use_Clause (Renamed_Entity (Pack), Empty);
            end if;

            if Chars (Pack) = Name_System
              and then Scope (Pack) = Standard_Standard
              and then Present_System_Aux
            then
               Id := First_Entity (System_Aux_Id);
               while Present (Id) loop
                  Set_Is_Potentially_Use_Visible (Id, False);

                  if Is_Private_Type (Id)
                    and then Present (Full_View (Id))
                  then
                     Set_Is_Potentially_Use_Visible (Full_View (Id), False);
                  end if;

                  Next_Entity (Id);
               end loop;

               Set_In_Use (System_Aux_Id, False);
            end if;
         else
            Set_Redundant_Use (Pack_Name, False);
         end if;
      end if;

      if Present (Hidden_By_Use_Clause (N)) then
         Elmt := First_Elmt (Hidden_By_Use_Clause (N));
         while Present (Elmt) loop
            declare
               E : constant Entity_Id := Node (Elmt);

            begin
               --  Reset either Use_Visibility or Direct_Visibility, depending
               --  on how the entity was hidden by the use clause.

               if In_Use (Scope (E))
                 and then Used_As_Generic_Actual (Scope (E))
               then
                  Set_Is_Potentially_Use_Visible (Node (Elmt));
               else
                  Set_Is_Immediately_Visible (Node (Elmt));
               end if;

               Next_Elmt (Elmt);
            end;
         end loop;

         Set_Hidden_By_Use_Clause (N, No_Elist);
      end if;
   end End_Use_Package;

   ------------------
   -- End_Use_Type --
   ------------------

   procedure End_Use_Type (N : Node_Id) is
      Elmt : Elmt_Id;
      Id   : Entity_Id;
      T    : Entity_Id;

   --  Start of processing for End_Use_Type

   begin
      Id := Subtype_Mark (N);

      --  A call to Rtsfind may occur while analyzing a use_type_clause, in
      --  which case the type marks are not resolved yet, so guard against that
      --  here.

      if Is_Entity_Name (Id) and then Present (Entity (Id)) then
         T := Entity (Id);

         if T = Any_Type or else From_Limited_With (T) then
            null;

         --  Note that the use_type_clause may mention a subtype of the type
         --  whose primitive operations have been made visible. Here as
         --  elsewhere, it is the base type that matters for visibility.

         elsif In_Open_Scopes (Scope (Base_Type (T))) then
            null;

         elsif not Redundant_Use (Id) then
            Set_In_Use (T, False);
            Set_In_Use (Base_Type (T), False);
            Set_Current_Use_Clause (T, Empty);
            Set_Current_Use_Clause (Base_Type (T), Empty);

            --  See Use_One_Type for the rationale. This is a bit on the naive
            --  side, but should be good enough in practice.

            if Is_Tagged_Type (T) then
               Set_In_Use (Class_Wide_Type (T), False);
            end if;
         end if;
      end if;

      if Is_Empty_Elmt_List (Used_Operations (N)) then
         return;

      else
         Elmt := First_Elmt (Used_Operations (N));
         while Present (Elmt) loop
            Set_Is_Potentially_Use_Visible (Node (Elmt), False);
            Next_Elmt (Elmt);
         end loop;
      end if;
   end End_Use_Type;

   --------------------
   -- Entity_Of_Unit --
   --------------------

   function Entity_Of_Unit (U : Node_Id) return Entity_Id is
   begin
      if Nkind (U) = N_Package_Instantiation and then Analyzed (U) then
         return Defining_Entity (Instance_Spec (U));
      else
         return Defining_Entity (U);
      end if;
   end Entity_Of_Unit;

   --------------------------------------
   -- Error_Missing_With_Of_Known_Unit --
   --------------------------------------

   procedure Error_Missing_With_Of_Known_Unit (Pkg : Node_Id) is
      Selectors : array (1 .. 6) of Node_Id;
      --  Contains the chars of the full package name up to maximum number
      --  allowed as per Errout.Error_Msg_Name_# variables.

      Count : Integer := Selectors'First;
      --  Count of selector names forming the full package name

      Current_Pkg : Node_Id := Parent (Pkg);

   begin
      Selectors (Count) := Pkg;

      --  Gather all the selectors we can display

      while Nkind (Current_Pkg) = N_Selected_Component
        and then Is_Known_Unit (Current_Pkg)
        and then Count < Selectors'Length
      loop
         Count             := Count + 1;
         Selectors (Count) := Selector_Name (Current_Pkg);
         Current_Pkg       := Parent (Current_Pkg);
      end loop;

      --  Display the error message based on the number of selectors found

      case Count is
         when 1 =>
            Error_Msg_Node_1 := Selectors (1);
            Error_Msg_N -- CODEFIX
              ("\\missing `WITH &;`", Pkg);
         when 2 =>
            Error_Msg_Node_1 := Selectors (1);
            Error_Msg_Node_2 := Selectors (2);
            Error_Msg_N -- CODEFIX
              ("\\missing `WITH &.&;`", Pkg);
         when 3 =>
            Error_Msg_Node_1 := Selectors (1);
            Error_Msg_Node_2 := Selectors (2);
            Error_Msg_Node_3 := Selectors (3);
            Error_Msg_N -- CODEFIX
              ("\\missing `WITH &.&.&;`", Pkg);
         when 4 =>
            Error_Msg_Node_1 := Selectors (1);
            Error_Msg_Node_2 := Selectors (2);
            Error_Msg_Node_3 := Selectors (3);
            Error_Msg_Node_3 := Selectors (4);
            Error_Msg_N -- CODEFIX
              ("\\missing `WITH &.&.&.&;`", Pkg);
         when 5 =>
            Error_Msg_Node_1 := Selectors (1);
            Error_Msg_Node_2 := Selectors (2);
            Error_Msg_Node_3 := Selectors (3);
            Error_Msg_Node_3 := Selectors (4);
            Error_Msg_Node_3 := Selectors (5);
            Error_Msg_N -- CODEFIX
              ("\\missing `WITH &.&.&.&.&;`", Pkg);
         when 6 =>
            Error_Msg_Node_1 := Selectors (1);
            Error_Msg_Node_2 := Selectors (2);
            Error_Msg_Node_3 := Selectors (3);
            Error_Msg_Node_4 := Selectors (4);
            Error_Msg_Node_5 := Selectors (5);
            Error_Msg_Node_6 := Selectors (6);
            Error_Msg_N -- CODEFIX
              ("\\missing `WITH &.&.&.&.&.&;`", Pkg);
         when others =>
            raise Program_Error;
      end case;
   end Error_Missing_With_Of_Known_Unit;

   --------------------
   -- Is_Self_Hidden --
   --------------------

   function Is_Self_Hidden (E : Entity_Id) return Boolean is
   begin
      if Is_Not_Self_Hidden (E) then
         return Ekind (E) = E_Void;
      else
         return True;
      end if;
   end Is_Self_Hidden;

   ----------------------
   -- Find_Direct_Name --
   ----------------------

   procedure Find_Direct_Name (N : Node_Id) is
      E   : Entity_Id;
      E2  : Entity_Id;
      Msg : Boolean;

      Homonyms : Entity_Id;
      --  Saves start of homonym chain

      Inst : Entity_Id := Empty;
      --  Enclosing instance, if any

      Nvis_Entity : Boolean;
      --  Set True to indicate that there is at least one entity on the homonym
      --  chain which, while not visible, is visible enough from the user point
      --  of view to warrant an error message of "not visible" rather than
      --  undefined.

      Nvis_Is_Private_Subprg : Boolean := False;
      --  Ada 2005 (AI-262): Set True to indicate that a form of Beaujolais
      --  effect concerning library subprograms has been detected. Used to
      --  generate the precise error message.

      function From_Actual_Package (E : Entity_Id) return Boolean;
      --  Returns true if the entity is an actual for a package that is itself
      --  an actual for a formal package of the current instance. Such an
      --  entity requires special handling because it may be use-visible but
      --  hides directly visible entities defined outside the instance, because
      --  the corresponding formal did so in the generic.

      function Is_Actual_Parameter return Boolean;
      --  This function checks if the node N is an identifier that is an actual
      --  parameter of a procedure call. If so it returns True, otherwise it
      --  return False. The reason for this check is that at this stage we do
      --  not know what procedure is being called if the procedure might be
      --  overloaded, so it is premature to go setting referenced flags or
      --  making calls to Generate_Reference. We will wait till Resolve_Actuals
      --  for that processing.
      --  Note: there is a similar routine Sem_Util.Is_Actual_Parameter, but
      --  it works for both function and procedure calls, while here we are
      --  only concerned with procedure calls (and with entry calls as well,
      --  but they are parsed as procedure calls and only later rewritten to
      --  entry calls).

      function Known_But_Invisible (E : Entity_Id) return Boolean;
      --  This function determines whether a reference to the entity E, which
      --  is not visible, can reasonably be considered to be known to the
      --  writer of the reference. This is a heuristic test, used only for
      --  the purposes of figuring out whether we prefer to complain that an
      --  entity is undefined or invisible (and identify the declaration of
      --  the invisible entity in the latter case). The point here is that we
      --  don't want to complain that something is invisible and then point to
      --  something entirely mysterious to the writer.

      procedure Nvis_Messages;
      --  Called if there are no visible entries for N, but there is at least
      --  one non-directly visible, or hidden declaration. This procedure
      --  outputs an appropriate set of error messages.

      procedure Undefined (Nvis : Boolean);
      --  This function is called if the current node has no corresponding
      --  visible entity or entities. The value set in Msg indicates whether
      --  an error message was generated (multiple error messages for the
      --  same variable are generally suppressed, see body for details).
      --  Msg is True if an error message was generated, False if not. This
      --  value is used by the caller to determine whether or not to output
      --  additional messages where appropriate. The parameter is set False
      --  to get the message "X is undefined", and True to get the message
      --  "X is not visible".

      -------------------------
      -- From_Actual_Package --
      -------------------------

      function From_Actual_Package (E : Entity_Id) return Boolean is
         Scop : constant Entity_Id := Scope (E);
         --  Declared scope of candidate entity

         function Declared_In_Actual (Pack : Entity_Id) return Boolean;
         --  Recursive function that does the work and examines actuals of
         --  actual packages of current instance.

         ------------------------
         -- Declared_In_Actual --
         ------------------------

         function Declared_In_Actual (Pack : Entity_Id) return Boolean is
            pragma Assert (Ekind (Pack) = E_Package);
            Act : Entity_Id;
         begin
            if No (Associated_Formal_Package (Pack)) then
               return False;

            else
               Act := First_Entity (Pack);
               while Present (Act) loop
                  if Renamed_Entity (Pack) = Scop then
                     return True;

                  --  Check for end of list of actuals

                  elsif Ekind (Act) = E_Package
                    and then Renamed_Entity (Act) = Pack
                  then
                     return False;

                  elsif Ekind (Act) = E_Package
                    and then Declared_In_Actual (Act)
                  then
                     return True;
                  end if;

                  Next_Entity (Act);
               end loop;

               return False;
            end if;
         end Declared_In_Actual;

         --  Local variables

         Act : Entity_Id;

      --  Start of processing for From_Actual_Package

      begin
         if not In_Instance then
            return False;

         else
            Inst := Current_Scope;
            while Present (Inst)
              and then Ekind (Inst) /= E_Package
              and then not Is_Generic_Instance (Inst)
            loop
               Inst := Scope (Inst);
            end loop;

            if No (Inst) then
               return False;
            end if;

            Act := First_Entity (Inst);
            while Present (Act) loop
               if Ekind (Act) = E_Package
                 and then Declared_In_Actual (Act)
               then
                  return True;
               end if;

               Next_Entity (Act);
            end loop;

            return False;
         end if;
      end From_Actual_Package;

      -------------------------
      -- Is_Actual_Parameter --
      -------------------------

      function Is_Actual_Parameter return Boolean is
      begin
         if Nkind (N) = N_Identifier then
            case Nkind (Parent (N)) is
               when N_Procedure_Call_Statement =>
                  return Is_List_Member (N)
                    and then List_Containing (N) =
                      Parameter_Associations (Parent (N));

               when N_Parameter_Association =>
                  return N = Explicit_Actual_Parameter (Parent (N))
                    and then Nkind (Parent (Parent (N))) =
                               N_Procedure_Call_Statement;

               when others =>
                  return False;
            end case;
         else
            return False;
         end if;
      end Is_Actual_Parameter;

      -------------------------
      -- Known_But_Invisible --
      -------------------------

      function Known_But_Invisible (E : Entity_Id) return Boolean is
         Fname : File_Name_Type;

      begin
         --  Entities in Standard are always considered to be known

         if Sloc (E) <= Standard_Location then
            return True;

         --  An entity that does not come from source is always considered
         --  to be unknown, since it is an artifact of code expansion.

         elsif not Comes_From_Source (E) then
            return False;
         end if;

         --  Here we have an entity that is not from package Standard, and
         --  which comes from Source. See if it comes from an internal file.

         Fname := Unit_File_Name (Get_Source_Unit (E));

         --  Case of from internal file

         if In_Internal_Unit (E) then

            --  Private part entities in internal files are never considered
            --  to be known to the writer of normal application code.

            if Is_Hidden (E) then
               return False;
            end if;

            --  Entities from System packages other than System and
            --  System.Storage_Elements are not considered to be known.
            --  System.Auxxxx files are also considered known to the user.

            --  Should refine this at some point to generally distinguish
            --  between known and unknown internal files ???

            Get_Name_String (Fname);

            return
              Name_Len < 2
                or else
              Name_Buffer (1 .. 2) /= "s-"
                or else
              Name_Buffer (3 .. 8) = "stoele"
                or else
              Name_Buffer (3 .. 5) = "aux";

         --  If not an internal file, then entity is definitely known, even if
         --  it is in a private part (the message generated will note that it
         --  is in a private part).

         else
            return True;
         end if;
      end Known_But_Invisible;

      -------------------
      -- Nvis_Messages --
      -------------------

      procedure Nvis_Messages is
         Comp_Unit : Node_Id;
         Ent       : Entity_Id;
         Found     : Boolean := False;
         Hidden    : Boolean := False;
         Item      : Node_Id;

      begin
         --  Ada 2005 (AI-262): Generate a precise error concerning the
         --  Beaujolais effect that was previously detected

         if Nvis_Is_Private_Subprg then

            pragma Assert (Nkind (E2) = N_Defining_Identifier
                            and then Ekind (E2) = E_Function
                            and then Scope (E2) = Standard_Standard
                            and then Has_Private_With (E2));

            --  Find the sloc corresponding to the private with'ed unit

            Comp_Unit := Cunit (Current_Sem_Unit);
            Error_Msg_Sloc := No_Location;

            Item := First (Context_Items (Comp_Unit));
            while Present (Item) loop
               if Nkind (Item) = N_With_Clause
                 and then Private_Present (Item)
                 and then Entity (Name (Item)) = E2
               then
                  Error_Msg_Sloc := Sloc (Item);
                  exit;
               end if;

               Next (Item);
            end loop;

            pragma Assert (Error_Msg_Sloc /= No_Location);

            Error_Msg_N ("(Ada 2005): hidden by private with clause #", N);
            return;
         end if;

         Undefined (Nvis => True);

         if Msg then

            --  First loop does hidden declarations

            Ent := Homonyms;
            while Present (Ent) loop
               if Is_Potentially_Use_Visible (Ent) then
                  if not Hidden then
                     Error_Msg_N -- CODEFIX
                       ("multiple use clauses cause hiding!", N);
                     Hidden := True;
                  end if;

                  Error_Msg_Sloc := Sloc (Ent);
                  Error_Msg_N -- CODEFIX
                    ("hidden declaration#!", N);
               end if;

               Ent := Homonym (Ent);
            end loop;

            --  If we found hidden declarations, then that's enough, don't
            --  bother looking for non-visible declarations as well.

            if Hidden then
               return;
            end if;

            --  Second loop does non-directly visible declarations

            Ent := Homonyms;
            while Present (Ent) loop
               if not Is_Potentially_Use_Visible (Ent) then

                  --  Do not bother the user with unknown entities

                  if not Known_But_Invisible (Ent) then
                     goto Continue;
                  end if;

                  Error_Msg_Sloc := Sloc (Ent);

                  --  Output message noting that there is a non-visible
                  --  declaration, distinguishing the private part case.

                  if Is_Hidden (Ent) then
                     Error_Msg_N ("non-visible (private) declaration#!", N);

                  --  If the entity is declared in a generic package, it
                  --  cannot be visible, so there is no point in adding it
                  --  to the list of candidates if another homograph from a
                  --  non-generic package has been seen.

                  elsif Ekind (Scope (Ent)) = E_Generic_Package
                    and then Found
                  then
                     null;

                  else
                     --  When the entity comes from a generic instance the
                     --  normal error message machinery will give the line
                     --  number of the generic package and the location of
                     --  the generic instance, but not the name of the
                     --  the instance.

                     --  So, in order to give more descriptive error messages
                     --  in this case, we include the name of the generic
                     --  package.

                     if Is_Generic_Instance (Scope (Ent)) then
                        Error_Msg_Name_1 := Chars (Scope (Ent));
                        Error_Msg_N -- CODEFIX
                          ("non-visible declaration from %#!", N);

                     --  Otherwise print the message normally

                     else
                        Error_Msg_N -- CODEFIX
                          ("non-visible declaration#!", N);
                     end if;

                     if Ekind (Scope (Ent)) /= E_Generic_Package then
                        Found := True;
                     end if;

                     if Is_Compilation_Unit (Ent)
                       and then
                         Nkind (Parent (Parent (N))) = N_Use_Package_Clause
                     then
                        Error_Msg_Qual_Level := 99;
                        Error_Msg_NE -- CODEFIX
                          ("\\missing `WITH &;`", N, Ent);
                        Error_Msg_Qual_Level := 0;
                     end if;

                     if Ekind (Ent) = E_Discriminant
                       and then Present (Corresponding_Discriminant (Ent))
                       and then Scope (Corresponding_Discriminant (Ent)) =
                                                        Etype (Scope (Ent))
                     then
                        Error_Msg_N
                          ("inherited discriminant not allowed here" &
                            " (RM 3.8 (12), 3.8.1 (6))!", N);
                     end if;
                  end if;

                  --  Set entity and its containing package as referenced. We
                  --  can't be sure of this, but this seems a better choice
                  --  to avoid unused entity messages.

                  if Comes_From_Source (Ent) then
                     Set_Referenced (Ent);
                     Set_Referenced (Cunit_Entity (Get_Source_Unit (Ent)));
                  end if;
               end if;

               <<Continue>>
               Ent := Homonym (Ent);
            end loop;
         end if;
      end Nvis_Messages;

      ---------------
      -- Undefined --
      ---------------

      procedure Undefined (Nvis : Boolean) is
         Emsg : Error_Msg_Id;

      begin
         --  We should never find an undefined internal name. If we do, then
         --  see if we have previous errors. If so, ignore on the grounds that
         --  it is probably a cascaded message (e.g. a block label from a badly
         --  formed block). If no previous errors, then we have a real internal
         --  error of some kind so raise an exception.

         if Is_Internal_Name (Chars (N)) then
            if Total_Errors_Detected /= 0 then
               return;
            else
               raise Program_Error;
            end if;
         end if;

         --  A very specialized error check, if the undefined variable is
         --  a case tag, and the case type is an enumeration type, check
         --  for a possible misspelling, and if so, modify the identifier

         --  Named aggregate should also be handled similarly ???

         if Nkind (N) = N_Identifier
           and then Nkind (Parent (N)) = N_Case_Statement_Alternative
         then
            declare
               Case_Stm : constant Node_Id   := Parent (Parent (N));
               Case_Typ : constant Entity_Id := Etype (Expression (Case_Stm));

               Lit : Node_Id;

            begin
               if Is_Enumeration_Type (Case_Typ)
                 and then not Is_Standard_Character_Type (Case_Typ)
               then
                  Lit := First_Literal (Case_Typ);
                  Get_Name_String (Chars (Lit));

                  if Chars (Lit) /= Chars (N)
                    and then Is_Bad_Spelling_Of (Chars (N), Chars (Lit))
                  then
                     Error_Msg_Node_2 := Lit;
                     Error_Msg_N -- CODEFIX
                       ("& is undefined, assume misspelling of &", N);
                     Rewrite (N, New_Occurrence_Of (Lit, Sloc (N)));
                     return;
                  end if;

                  Next_Literal (Lit);
               end if;
            end;
         end if;

         --  Normal processing

         Set_Entity (N, Any_Id);
         Set_Etype  (N, Any_Type);

         --  We use the table Urefs to keep track of entities for which we
         --  have issued errors for undefined references. Multiple errors
         --  for a single name are normally suppressed, however we modify
         --  the error message to alert the programmer to this effect.

         for J in Urefs.First .. Urefs.Last loop
            if Chars (N) = Chars (Urefs.Table (J).Node) then
               if Urefs.Table (J).Err /= No_Error_Msg
                 and then Sloc (N) /= Urefs.Table (J).Loc
               then
                  Error_Msg_Node_1 := Urefs.Table (J).Node;

                  if Urefs.Table (J).Nvis then
                     Change_Error_Text (Urefs.Table (J).Err,
                       "& is not visible (more references follow)");
                  else
                     Change_Error_Text (Urefs.Table (J).Err,
                       "& is undefined (more references follow)");
                  end if;

                  Urefs.Table (J).Err := No_Error_Msg;
               end if;

               --  Although we will set Msg False, and thus suppress the
               --  message, we also set Error_Posted True, to avoid any
               --  cascaded messages resulting from the undefined reference.

               Msg := False;
               Set_Error_Posted (N);
               return;
            end if;
         end loop;

         --  If entry not found, this is first undefined occurrence

         if Nvis then
            Error_Msg_N ("& is not visible!", N);
            Emsg := Get_Msg_Id;

         else
            Error_Msg_N ("& is undefined!", N);
            Emsg := Get_Msg_Id;

            --  A very bizarre special check, if the undefined identifier
            --  is Put or Put_Line, then add a special error message (since
            --  this is a very common error for beginners to make).

            if Chars (N) in Name_Put | Name_Put_Line then
               Error_Msg_N -- CODEFIX
                 ("\\possible missing `WITH Ada.Text_'I'O; " &
                  "USE Ada.Text_'I'O`!", N);

            --  Another special check if N is the prefix of a selected
            --  component which is a known unit: add message complaining
            --  about missing with for this unit.

            elsif Nkind (Parent (N)) = N_Selected_Component
              and then N = Prefix (Parent (N))
              and then Is_Known_Unit (Parent (N))
            then
               Error_Missing_With_Of_Known_Unit (N);
            end if;

            --  Now check for possible misspellings

            declare
               E      : Entity_Id;
               Ematch : Entity_Id := Empty;
            begin
               for Nam in First_Name_Id .. Last_Name_Id loop
                  E := Get_Name_Entity_Id (Nam);

                  if Present (E)
                     and then (Is_Immediately_Visible (E)
                                 or else
                               Is_Potentially_Use_Visible (E))
                  then
                     if Is_Bad_Spelling_Of (Chars (N), Nam) then
                        Ematch := E;
                        exit;
                     end if;
                  end if;
               end loop;

               if Present (Ematch) then
                  Error_Msg_NE -- CODEFIX
                    ("\possible misspelling of&", N, Ematch);
               end if;
            end;
         end if;

         --  Make entry in undefined references table unless the full errors
         --  switch is set, in which case by refraining from generating the
         --  table entry we guarantee that we get an error message for every
         --  undefined reference. The entry is not added if we are ignoring
         --  errors.

         if not All_Errors_Mode
           and then Ignore_Errors_Enable = 0
           and then not Get_Ignore_Errors
         then
            Urefs.Append (
              (Node => N,
               Err  => Emsg,
               Nvis => Nvis,
               Loc  => Sloc (N)));
         end if;

         Msg := True;
      end Undefined;

      --  Local variables

      Nested_Inst : Entity_Id := Empty;
      --  The entity of a nested instance which appears within Inst (if any)

   --  Start of processing for Find_Direct_Name

   begin
      --  If the entity pointer is already set, this is an internal node, or
      --  a node that is analyzed more than once, after a tree modification.
      --  In such a case there is no resolution to perform, just set the type.

      if Present (Entity (N)) then
         if Is_Type (Entity (N)) then
            Set_Etype (N, Entity (N));

         else
            declare
               Entyp : constant Entity_Id := Etype (Entity (N));

            begin
               --  One special case here. If the Etype field is already set,
               --  and references the packed array type corresponding to the
               --  etype of the referenced entity, then leave it alone. This
               --  happens for trees generated from Exp_Pakd, where expressions
               --  can be deliberately "mis-typed" to the packed array type.

               if Is_Packed_Array (Entyp)
                 and then Present (Etype (N))
                 and then Etype (N) = Packed_Array_Impl_Type (Entyp)
               then
                  null;

               --  If not that special case, then just reset the Etype

               else
                  Set_Etype (N, Entyp);
               end if;
            end;
         end if;

         --  Although the marking of use clauses happens at the end of
         --  Find_Direct_Name, a certain case where a generic actual satisfies
         --  a use clause must be checked here due to how the generic machinery
         --  handles the analysis of said actuals.

         if In_Instance
           and then Nkind (Parent (N)) = N_Generic_Association
         then
            Mark_Use_Clauses (Entity (N));
         end if;

         return;
      end if;

      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      if Nkind (N) = N_Identifier then
         Mark_Elaboration_Attributes
           (N_Id     => N,
            Checks   => True,
            Modes    => True,
            Warnings => True);
      end if;

      --  Here if Entity pointer was not set, we need full visibility analysis
      --  First we generate debugging output if the debug E flag is set.

      if Debug_Flag_E then
         Write_Str ("Looking for ");
         Write_Name (Chars (N));
         Write_Eol;
      end if;

      Homonyms := Current_Entity (N);
      Nvis_Entity := False;

      E := Homonyms;
      while Present (E) loop

         --  If entity is immediately visible or potentially use visible, then
         --  process the entity and we are done.

         if Is_Immediately_Visible (E) then
            goto Immediately_Visible_Entity;

         elsif Is_Potentially_Use_Visible (E) then
            goto Potentially_Use_Visible_Entity;

         --  Note if a known but invisible entity encountered

         elsif Known_But_Invisible (E) then
            Nvis_Entity := True;
         end if;

         --  Move to next entity in chain and continue search

         E := Homonym (E);
      end loop;

      --  If no entries on homonym chain that were potentially visible,
      --  and no entities reasonably considered as non-visible, then
      --  we have a plain undefined reference, with no additional
      --  explanation required.

      if not Nvis_Entity then
         Undefined (Nvis => False);

      --  Otherwise there is at least one entry on the homonym chain that
      --  is reasonably considered as being known and non-visible.

      else
         Nvis_Messages;
      end if;

      goto Done;

      --  Processing for a potentially use visible entry found. We must search
      --  the rest of the homonym chain for two reasons. First, if there is a
      --  directly visible entry, then none of the potentially use-visible
      --  entities are directly visible (RM 8.4(10)). Second, we need to check
      --  for the case of multiple potentially use-visible entries hiding one
      --  another and as a result being non-directly visible (RM 8.4(11)).

      <<Potentially_Use_Visible_Entity>> declare
         Only_One_Visible : Boolean := True;
         All_Overloadable : Boolean := Is_Overloadable (E);

      begin
         E2 := Homonym (E);
         while Present (E2) loop
            if Is_Immediately_Visible (E2) then

               --  If the use-visible entity comes from the actual for a
               --  formal package, it hides a directly visible entity from
               --  outside the instance.

               if From_Actual_Package (E)
                 and then Scope_Depth (Scope (E2)) < Scope_Depth (Inst)
               then
                  goto Found;
               else
                  E := E2;
                  goto Immediately_Visible_Entity;
               end if;

            elsif Is_Potentially_Use_Visible (E2) then
               Only_One_Visible := False;
               All_Overloadable := All_Overloadable and Is_Overloadable (E2);

            --  Ada 2005 (AI-262): Protect against a form of Beaujolais effect
            --  that can occur in private_with clauses. Example:

            --    with A;
            --    private with B;              package A is
            --    package C is                   function B return Integer;
            --      use A;                     end A;
            --      V1 : Integer := B;
            --    private                      function B return Integer;
            --      V2 : Integer := B;
            --    end C;

            --  V1 resolves to A.B, but V2 resolves to library unit B

            elsif Ekind (E2) = E_Function
              and then Scope (E2) = Standard_Standard
              and then Has_Private_With (E2)
            then
               Only_One_Visible       := False;
               All_Overloadable       := False;
               Nvis_Is_Private_Subprg := True;
               exit;
            end if;

            E2 := Homonym (E2);
         end loop;

         --  On falling through this loop, we have checked that there are no
         --  immediately visible entities. Only_One_Visible is set if exactly
         --  one potentially use visible entity exists. All_Overloadable is
         --  set if all the potentially use visible entities are overloadable.
         --  The condition for legality is that either there is one potentially
         --  use visible entity, or if there is more than one, then all of them
         --  are overloadable.

         if Only_One_Visible or All_Overloadable then
            goto Found;

         --  If there is more than one potentially use-visible entity and at
         --  least one of them non-overloadable, we have an error (RM 8.4(11)).
         --  Note that E points to the first such entity on the homonym list.

         else
            --  If one of the entities is declared in an actual package, it
            --  was visible in the generic, and takes precedence over other
            --  entities that are potentially use-visible. The same applies
            --  if the entity is declared in a local instantiation of the
            --  current instance.

            if In_Instance then

               --  Find the current instance

               Inst := Current_Scope;
               while Present (Inst) and then Inst /= Standard_Standard loop
                  if Is_Generic_Instance (Inst) then
                     exit;
                  end if;

                  Inst := Scope (Inst);
               end loop;

               --  Reexamine the candidate entities, giving priority to those
               --  that were visible within the generic.

               E2 := E;
               while Present (E2) loop
                  Nested_Inst := Nearest_Enclosing_Instance (E2);

                  --  The entity is declared within an actual package, or in a
                  --  nested instance. The ">=" accounts for the case where the
                  --  current instance and the nested instance are the same.

                  if From_Actual_Package (E2)
                    or else (Present (Nested_Inst)
                              and then Scope_Depth (Nested_Inst) >=
                                       Scope_Depth (Inst))
                  then
                     E := E2;
                     goto Found;
                  end if;

                  E2 := Homonym (E2);
               end loop;

               Nvis_Messages;
               goto Done;

            elsif Is_Predefined_Unit (Current_Sem_Unit)
              and then not Is_Predefined_Unit (Main_Unit)
            then
               --  A use clause in the body of a system file creates conflict
               --  with some entity in a user scope, while rtsfind is active.
               --  Keep only the entity coming from another predefined unit.

               E2 := E;
               while Present (E2) loop
                  if In_Predefined_Unit (E2) then
                     E := E2;
                     goto Found;
                  end if;

                  E2 := Homonym (E2);
               end loop;

               --  Entity must exist because predefined unit is correct

               raise Program_Error;

            else
               Nvis_Messages;
               goto Done;
            end if;
         end if;
      end;

      --  Come here with E set to the first immediately visible entity on
      --  the homonym chain. This is the one we want unless there is another
      --  immediately visible entity further on in the chain for an inner
      --  scope (RM 8.3(8)).

      <<Immediately_Visible_Entity>> declare
         Level : Int;
         Scop  : Entity_Id;

      begin
         --  Find scope level of initial entity. When compiling through
         --  Rtsfind, the previous context is not completely invisible, and
         --  an outer entity may appear on the chain, whose scope is below
         --  the entry for Standard that delimits the current scope stack.
         --  Indicate that the level for this spurious entry is outside of
         --  the current scope stack.

         Level := Scope_Stack.Last;
         loop
            Scop := Scope_Stack.Table (Level).Entity;
            exit when Scop = Scope (E);
            Level := Level - 1;
            exit when Scop = Standard_Standard;
         end loop;

         --  Now search remainder of homonym chain for more inner entry
         --  If the entity is Standard itself, it has no scope, and we
         --  compare it with the stack entry directly.

         E2 := Homonym (E);
         while Present (E2) loop
            if Is_Immediately_Visible (E2) then

               --  If a generic package contains a local declaration that
               --  has the same name as the generic, there may be a visibility
               --  conflict in an instance, where the local declaration must
               --  also hide the name of the corresponding package renaming.
               --  We check explicitly for a package declared by a renaming,
               --  whose renamed entity is an instance that is on the scope
               --  stack, and that contains a homonym in the same scope. Once
               --  we have found it, we know that the package renaming is not
               --  immediately visible, and that the identifier denotes the
               --  other entity (and its homonyms if overloaded).

               if Scope (E) = Scope (E2)
                 and then Ekind (E) = E_Package
                 and then Present (Renamed_Entity (E))
                 and then Is_Generic_Instance (Renamed_Entity (E))
                 and then In_Open_Scopes (Renamed_Entity (E))
                 and then Comes_From_Source (N)
               then
                  Set_Is_Immediately_Visible (E, False);
                  E := E2;

               else
                  for J in Level + 1 .. Scope_Stack.Last loop
                     if Scope_Stack.Table (J).Entity = Scope (E2)
                       or else Scope_Stack.Table (J).Entity = E2
                     then
                        Level := J;
                        E := E2;
                        exit;
                     end if;
                  end loop;
               end if;
            end if;

            E2 := Homonym (E2);
         end loop;

         --  At the end of that loop, E is the innermost immediately
         --  visible entity, so we are all set.
      end;

      --  Come here with entity found, and stored in E

      <<Found>> begin

         --  Check violation of No_Wide_Characters restriction

         Check_Wide_Character_Restriction (E, N);

         --  When distribution features are available (Get_PCS_Name /=
         --  Name_No_DSA), a remote access-to-subprogram type is converted
         --  into a record type holding whatever information is needed to
         --  perform a remote call on an RCI subprogram. In that case we
         --  rewrite any occurrence of the RAS type into the equivalent record
         --  type here. 'Access attribute references and RAS dereferences are
         --  then implemented using specific TSSs. However when distribution is
         --  not available (case of Get_PCS_Name = Name_No_DSA), we bypass the
         --  generation of these TSSs, and we must keep the RAS type in its
         --  original access-to-subprogram form (since all calls through a
         --  value of such type will be local anyway in the absence of a PCS).

         if Comes_From_Source (N)
           and then Is_Remote_Access_To_Subprogram_Type (E)
           and then Ekind (E) = E_Access_Subprogram_Type
           and then Expander_Active
           and then Get_PCS_Name /= Name_No_DSA
         then
            Rewrite (N, New_Occurrence_Of (Equivalent_Type (E), Sloc (N)));
            goto Done;
         end if;

         --  Set the entity. Note that the reason we call Set_Entity for the
         --  overloadable case, as opposed to Set_Entity_With_Checks is
         --  that in the overloaded case, the initial call can set the wrong
         --  homonym. The call that sets the right homonym is in Sem_Res and
         --  that call does use Set_Entity_With_Checks, so we don't miss
         --  a style check.

         if Is_Overloadable (E) then
            Set_Entity (N, E);
         else
            Set_Entity_With_Checks (N, E);
         end if;

         if Is_Type (E) then
            Set_Etype (N, E);
         else
            Set_Etype (N, Get_Full_View (Etype (E)));
         end if;

         if Debug_Flag_E then
            Write_Str (" found  ");
            Write_Entity_Info (E, "      ");
         end if;

         if Is_Self_Hidden (E)
           and then
             (not Is_Record_Type (Current_Scope)
               or else Nkind (Parent (N)) /= N_Pragma_Argument_Association)
         then
            Premature_Usage (N);

         --  If the entity is overloadable, collect all interpretations of the
         --  name for subsequent overload resolution. We optimize a bit here to
         --  do this only if we have an overloadable entity that is not on its
         --  own on the homonym chain.

         elsif Is_Overloadable (E)
           and then (Present (Homonym (E)) or else Current_Entity (N) /= E)
         then
            Collect_Interps (N);

            --  Background: for an instance of a generic, expansion sets
            --  entity fields on names that refer to things declared
            --  outside of the instance, but leaves the entity field
            --  unset on names that should end up referring to things
            --  declared within the instance. These will instead be set by
            --  analysis - the idea is that if a name resolves a certain
            --  way in the generic, then we should get corresponding results
            --  if we resolve the corresponding name in an instance. For this
            --  to work, we have to prevent unrelated declarations that
            --  happen to be visible at the point of the instantiation from
            --  participating in resolution and causing problems (typically
            --  ambiguities, but incorrect resolutions are also probably
            --  possible). So here we filter out such unwanted interpretations.
            --
            --  Note that there are other problems with this approach to
            --  implementing generic instances that are not addressed here.
            --  Inside a generic, we might have no trouble resolving a call
            --  where the two candidates are a function that returns a
            --  formal type and a function that returns Standard.Integer.
            --  If we instantiate that generic and the corresponding actual
            --  type is Standard.Integer, then we may incorrectly reject the
            --  corresponding call in the instance as ambiguous (or worse,
            --  we may quietly choose the wrong resolution).
            --
            --  Another such problem can occur with a type derived from a
            --  formal derived type. In an instance, such a type may have
            --  inherited subprograms that are not present in the generic.
            --  These can then interfere with name resolution (e.g., if
            --  some declaration is visible via a use-clause in the generic
            --  and some name in the generic refers to it, then the
            --  corresponding declaration in an instance may be hidden by
            --  a directly visible inherited subprogram and the corresponding
            --  name in the instance may then incorrectly refer to the
            --  inherited subprogram).

            if In_Instance then
               declare
                  function Is_Actual_Subp_Of_Inst
                    (E : Entity_Id; Inst : Entity_Id) return Boolean;
                  --  Return True if E is an actual parameter
                  --  corresponding to a formal subprogram of the
                  --  instantiation Inst.

                  function Is_Extraneously_Visible
                    (E : Entity_Id; Inst : Entity_Id) return Boolean;
                  --  Return True if E is an interpretation that should
                  --  be filtered out. That is, if E is an "unwanted"
                  --  resolution candidate as described in the
                  --  preceding "Background:" commment.

                  function Is_Generic_Actual_Subp_Name
                    (N : Node_Id) return Boolean;
                  --  Return True if N is the name of a subprogram
                  --  renaming generated for a generic actual.

                  ----------------------------
                  -- Is_Actual_Subp_Of_Inst --
                  ----------------------------

                  function Is_Actual_Subp_Of_Inst
                    (E : Entity_Id; Inst : Entity_Id) return Boolean
                  is
                     Decl                              : Node_Id;
                     Generic_From_E, Generic_From_Inst : Entity_Id;
                  begin
                     --  ???
                     --  Why is Is_Generic_Actual_Subprogram undefined
                     --  in the E_Operator case?

                     if Ekind (E) not in E_Function | E_Procedure
                       or else not Is_Generic_Actual_Subprogram (E)
                     then
                        return False;
                     end if;

                     Decl := Enclosing_Declaration (E);

                     --  Look for the suprogram renaming declaration built
                     --  for a generic actual subprogram. Unclear why
                     --  Original_Node call is needed, but sometimes it is.

                     if Decl not in N_Subprogram_Renaming_Declaration_Id then
                        Decl := Original_Node (Decl);
                     end if;

                     if Decl in N_Subprogram_Renaming_Declaration_Id then
                        Generic_From_E :=
                          Scope (Corresponding_Formal_Spec (Decl));
                     else
                        --  ??? In the case of a generic formal subprogram
                        --  which has a pre/post condition, it is unclear how
                        --  to find the Corresponding_Formal_Spec-bearing node.

                        Generic_From_E := Empty;
                     end if;

                     declare
                        Inst_Parent : Node_Id := Parent (Inst);
                     begin
                        if Nkind (Inst_Parent) = N_Defining_Program_Unit_Name
                        then
                           Inst_Parent := Parent (Inst_Parent);
                        end if;

                        Generic_From_Inst := Generic_Parent (Inst_Parent);
                     end;

                     return Generic_From_E = Generic_From_Inst
                       and then Present (Generic_From_E);
                  end Is_Actual_Subp_Of_Inst;

                  -----------------------------
                  -- Is_Extraneously_Visible --
                  -----------------------------

                  function Is_Extraneously_Visible
                    (E : Entity_Id; Inst : Entity_Id) return Boolean is
                  begin
                     --  Return False in various non-extraneous cases.
                     --  If none of those apply, then return True.

                     if Within_Scope (E, Inst) then
                        --  return False if E declared within Inst
                        return False;

                     elsif Is_Actual_Subp_Of_Inst (E, Inst) then
                        --  Return False if E is an actual subprogram,
                        --  and therefore may be referenced within Inst.
                        return False;

                     elsif Nkind (Parent (E)) = N_Subtype_Declaration
                        and then Defining_Identifier (Parent (E)) /= E
                     then
                        --  Return False for a primitive subp of an
                        --  actual corresponding to a formal type.

                        return False;

                     elsif not In_Open_Scopes (Scope (E)) then
                        --  Return False if this candidate is not
                        --  declared in a currently open scope.

                        return False;

                     else
                        declare
                           --  We want to know whether the declaration of
                           --  E comes textually after the declaration of
                           --  the generic that Inst is an instance of
                           --  (and after the generic body if there is one).
                           --  To compare, we climb up the deeper of the two
                           --  scope chains until we the levels match.
                           --  There is a separate loop for each starting
                           --  point, but we will execute zero iterations
                           --  for at least one of the two loops.
                           --  For each Xxx_Scope, we have a corresponding
                           --  Xxx_Trailer; the latter is the predecessor of
                           --  the former in the scope traversal.

                           E_Trailer : Entity_Id := E;
                           E_Scope : Entity_Id := Scope (E);
                           pragma Assert (Present (E_Scope));

                           --  the generic that Inst is an instance of
                           Gen_Trailer : Entity_Id :=
                             Generic_Parent (Specification
                               (Unit_Declaration_Node (Inst)));
                           Gen_Scope : Entity_Id;

                           function Has_Formal_Package_Parameter
                             (Generic_Id : Entity_Id) return Boolean;
                           --  Return True iff given generic has at least one
                           --  formal package parameter.

                           ----------------------------------
                           -- Has_Formal_Package_Parameter --
                           ----------------------------------

                           function Has_Formal_Package_Parameter
                             (Generic_Id : Entity_Id) return Boolean is
                              Formal_Decl : Node_Id :=
                                First (Generic_Formal_Declarations
                                  (Enclosing_Generic_Unit (Generic_Id)));
                           begin
                              while Present (Formal_Decl) loop
                                 if Nkind (Original_Node (Formal_Decl)) =
                                   N_Formal_Package_Declaration
                                 then
                                    return True;
                                 end if;

                                 Next (Formal_Decl);
                              end loop;
                              return False;
                           end Has_Formal_Package_Parameter;

                        begin
                           if No (Gen_Trailer) then
                              --  Dunno how this can happen, but it can.
                              return False;
                           else
                              if Has_Formal_Package_Parameter (Gen_Trailer)
                              then
                                 --  Punt on sorting out what is visible via a
                                 --  formal package.

                                 return False;
                              end if;

                              if Is_Child_Unit (Gen_Trailer)
                                and then Is_Generic_Unit
                                           (Entity (Name
                                             (Parent (Gen_Trailer))))
                              then
                                 --  Punt on dealing with how the FE fails
                                 --  to build a tree for a "sprouted" generic
                                 --  so that what should be a reference to
                                 --  I1.G2 instead points into G1.G2 .

                                 return False;
                              end if;

                              Gen_Scope := Scope (Gen_Trailer);

                              while Scope_Depth (E_Scope)
                                      > Scope_Depth (Gen_Scope)
                              loop
                                 E_Trailer := E_Scope;
                                 E_Scope := Scope (E_Scope);
                              end loop;
                              while Scope_Depth (E_Scope)
                                      < Scope_Depth (Gen_Scope)
                              loop
                                 Gen_Trailer := Gen_Scope;
                                 Gen_Scope := Scope (Gen_Scope);
                              end loop;
                           end if;

                           if Gen_Scope = E_Scope then
                              --  if Gen_Trailer and E_Trailer are declared
                              --  in the same declarative part and E_Trailer
                              --  occurs after the declaration (and body, if
                              --  there is one) of Gen_Trailer, then
                              --  return True because E was declared after
                              --  the generic that Inst is an instance of
                              --  (and also after that generic's body, if it
                              --  has one).

                              if Is_Package_Or_Generic_Package (Gen_Trailer)
                                and then Present (Package_Body (Gen_Trailer))
                              then
                                 Gen_Trailer :=
                                   Corresponding_Body
                                     (Package_Spec (Gen_Trailer));
                              end if;

                              declare
                                 Id : Entity_Id := Gen_Trailer;
                              begin
                                 loop
                                    if No (Id) then
                                       --  E_Trailer presumably occurred
                                       --  earlier on the entity list than
                                       --  Gen_Trailer. So E preceded the
                                       --  generic that Inst is an instance
                                       --  of (or the body of that generic if
                                       --  it has one) and so could have
                                       --  been referenced within the generic.
                                       return False;
                                    end if;
                                    exit when Id = E_Trailer;
                                    Next_Entity (Id);
                                 end loop;
                              end;
                           end if;
                        end;
                     end if;

                     if Present (Nearest_Enclosing_Instance (Inst)) then
                        return Is_Extraneously_Visible
                          (E => E, Inst => Nearest_Enclosing_Instance (Inst));

                     --  The preceding Nearest_Enclosing_Instance test
                     --  doesn't handle the case of an instance of a
                     --  "sprouted" generic. For example, if Inst=I2 in
                     --    generic package G1
                     --    generic package G1.G2;
                     --    package I1 is new G1;
                     --    package I2 is new I1.G2;
                     --  then N_E_I (Inst) = Empty. So deal with that case.

                     elsif Present (Nearest_Enclosing_Instance (E)) then
                        return Is_Extraneously_Visible
                          (E => Nearest_Enclosing_Instance (E),
                           Inst => Inst);
                     end if;

                     return True;
                  end Is_Extraneously_Visible;

                  ---------------------------------
                  -- Is_Generic_Actual_Subp_Name --
                  ---------------------------------

                  function Is_Generic_Actual_Subp_Name
                    (N : Node_Id) return Boolean
                  is
                     Decl : constant Node_Id := Enclosing_Declaration (N);
                  begin
                     return Nkind (Decl) = N_Subprogram_Renaming_Declaration
                       and then Present (Corresponding_Formal_Spec (Decl));
                  end Is_Generic_Actual_Subp_Name;

                  I    : Interp_Index;
                  It   : Interp;
                  Inst : Entity_Id := Current_Scope;

               begin
                  while Present (Inst)
                    and then not Is_Generic_Instance (Inst)
                  loop
                     Inst := Scope (Inst);
                  end loop;

                  if Present (Inst) then
                     Get_First_Interp (N, I, It);
                     while Present (It.Nam) loop
                        if Is_Extraneously_Visible (E => It.Nam, Inst => Inst)
                          and then not Is_Generic_Actual_Subp_Name (N)
                        then
                           Remove_Interp (I);
                        end if;
                        Get_Next_Interp (I, It);
                     end loop;
                  end if;
               end;
            end if;

            --  If no homonyms were visible, the entity is unambiguous

            if not Is_Overloaded (N) then
               if not Is_Actual_Parameter then
                  Generate_Reference (E, N);
               end if;
            end if;

         --  Case of non-overloadable entity, set the entity providing that
         --  we do not have the case of a discriminant reference within a
         --  default expression. Such references are replaced with the
         --  corresponding discriminal, which is the formal corresponding to
         --  to the discriminant in the initialization procedure.

         else
            --  Entity is unambiguous, indicate that it is referenced here

            --  For a renaming of an object, always generate simple reference,
            --  we don't try to keep track of assignments in this case, except
            --  in SPARK mode where renamings are traversed for generating
            --  local effects of subprograms.

            if Is_Object (E)
              and then Present (Renamed_Object (E))
              and then not GNATprove_Mode
            then
               Generate_Reference (E, N);

               --  If the renamed entity is a private protected component,
               --  reference the original component as well. This needs to be
               --  done because the private renamings are installed before any
               --  analysis has occurred. Reference to a private component will
               --  resolve to the renaming and the original component will be
               --  left unreferenced, hence the following.

               if Is_Prival (E) then
                  Generate_Reference (Prival_Link (E), N);
               end if;

            --  One odd case is that we do not want to set the Referenced flag
            --  if the entity is a label, and the identifier is the label in
            --  the source, since this is not a reference from the point of
            --  view of the user.

            elsif Nkind (Parent (N)) = N_Label then
               declare
                  R : constant Boolean := Referenced (E);

               begin
                  --  Generate reference unless this is an actual parameter
                  --  (see comment below).

                  if not Is_Actual_Parameter then
                     Generate_Reference (E, N);
                     Set_Referenced (E, R);
                  end if;
               end;

            --  Normal case, not a label: generate reference

            else
               if not Is_Actual_Parameter then

                  --  Package or generic package is always a simple reference

                  if Is_Package_Or_Generic_Package (E) then
                     Generate_Reference (E, N, 'r');

                  --  Else see if we have a left hand side

                  else
                     case Known_To_Be_Assigned (N, Only_LHS => True) is
                        when True =>
                           Generate_Reference (E, N, 'm');

                        when False =>
                           Generate_Reference (E, N, 'r');

                     end case;
                  end if;
               end if;
            end if;

            Set_Entity_Or_Discriminal (N, E);

            --  The name may designate a generalized reference, in which case
            --  the dereference interpretation will be included. Context is
            --  one in which a name is legal.

            if Ada_Version >= Ada_2012
              and then
                (Nkind (Parent (N)) in N_Subexpr
                  or else Nkind (Parent (N)) in N_Assignment_Statement
                                              | N_Object_Declaration
                                              | N_Parameter_Association)
            then
               Check_Implicit_Dereference (N, Etype (E));
            end if;
         end if;
      end;

      --  Mark relevant use-type and use-package clauses as effective if the
      --  node in question is not overloaded and therefore does not require
      --  resolution.
      --
      --  Note: Generic actual subprograms do not follow the normal resolution
      --  path, so ignore the fact that they are overloaded and mark them
      --  anyway.

      if Nkind (N) not in N_Subexpr or else not Is_Overloaded (N) then
         Mark_Use_Clauses (N);
      end if;

   --  Come here with entity set

   <<Done>>
      Check_Restriction_No_Use_Of_Entity (N);

      --  Annotate the tree by creating a variable reference marker in case the
      --  original variable reference is folded or optimized away. The variable
      --  reference marker is automatically saved for later examination by the
      --  ABE Processing phase. Variable references which act as actuals in a
      --  call require special processing and are left to Resolve_Actuals. The
      --  reference is a write when it appears on the left hand side of an
      --  assignment.

      if Needs_Variable_Reference_Marker (N => N, Calls_OK => False) then
         declare
            Is_Assignment_LHS : constant Boolean := Known_To_Be_Assigned (N);

         begin
            Build_Variable_Reference_Marker
              (N     => N,
               Read  => not Is_Assignment_LHS,
               Write => Is_Assignment_LHS);
         end;
      end if;
   end Find_Direct_Name;

   ------------------------
   -- Find_Expanded_Name --
   ------------------------

   --  This routine searches the homonym chain of the entity until it finds
   --  an entity declared in the scope denoted by the prefix. If the entity
   --  is private, it may nevertheless be immediately visible, if we are in
   --  the scope of its declaration.

   procedure Find_Expanded_Name (N : Node_Id) is
      function In_Abstract_View_Pragma (Nod : Node_Id) return Boolean;
      --  Determine whether expanded name Nod appears within a pragma which is
      --  a suitable context for an abstract view of a state or variable. The
      --  following pragmas fall in this category:
      --    Depends
      --    Global
      --    Initializes
      --    Refined_Depends
      --    Refined_Global
      --
      --  In addition, pragma Abstract_State is also considered suitable even
      --  though it is an illegal context for an abstract view as this allows
      --  for proper resolution of abstract views of variables. This illegal
      --  context is later flagged in the analysis of indicator Part_Of.

      -----------------------------
      -- In_Abstract_View_Pragma --
      -----------------------------

      function In_Abstract_View_Pragma (Nod : Node_Id) return Boolean is
         Par : Node_Id;

      begin
         --  Climb the parent chain looking for a pragma

         Par := Nod;
         while Present (Par) loop
            if Nkind (Par) = N_Pragma then
               if Pragma_Name_Unmapped (Par)
                    in Name_Abstract_State
                     | Name_Depends
                     | Name_Global
                     | Name_Initializes
                     | Name_Refined_Depends
                     | Name_Refined_Global
               then
                  return True;

               --  Otherwise the pragma is not a legal context for an abstract
               --  view.

               else
                  exit;
               end if;

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end In_Abstract_View_Pragma;

      --  Local variables

      Selector : constant Node_Id := Selector_Name (N);

      Candidate : Entity_Id := Empty;
      P_Name    : Entity_Id;
      Id        : Entity_Id;

   --  Start of processing for Find_Expanded_Name

   begin
      P_Name := Entity (Prefix (N));

      --  If the prefix is a renamed package, look for the entity in the
      --  original package.

      if Ekind (P_Name) = E_Package
        and then Present (Renamed_Entity (P_Name))
      then
         P_Name := Renamed_Entity (P_Name);

         if From_Limited_With (P_Name)
           and then not Unit_Is_Visible (Cunit (Get_Source_Unit (P_Name)))
         then
            Error_Msg_NE
              ("renaming of limited view of package & not usable in this"
               & " context (RM 8.5.3(3.1/2))", Prefix (N), P_Name);

         elsif Renames_Limited_View (Entity (Prefix (N)))
           and then not Unit_Is_Visible (Cunit (Get_Source_Unit (P_Name)))
           and then not Is_Visible_Through_Renamings (P_Name)
         then
            Error_Msg_NE
              ("renaming of limited view of package & not usable in this"
               & " context (RM 8.5.3(3.1/2))", Prefix (N), P_Name);
         end if;

         --  Rewrite node with entity field pointing to renamed object

         Rewrite (Prefix (N), New_Copy (Prefix (N)));
         Set_Entity (Prefix (N), P_Name);

      --  If the prefix is an object of a concurrent type, look for
      --  the entity in the associated task or protected type.

      elsif Is_Concurrent_Type (Etype (P_Name)) then
         P_Name := Etype (P_Name);
      end if;

      Id := Current_Entity (Selector);

      declare
         Is_New_Candidate : Boolean;

      begin
         while Present (Id) loop
            if Scope (Id) = P_Name then
               Candidate        := Id;
               Is_New_Candidate := True;

               --  Handle abstract views of states and variables. These are
               --  acceptable candidates only when the reference to the view
               --  appears in certain pragmas.

               if Ekind (Id) = E_Abstract_State
                 and then From_Limited_With (Id)
                 and then Present (Non_Limited_View (Id))
               then
                  if In_Abstract_View_Pragma (N) then
                     Candidate        := Non_Limited_View (Id);
                     Is_New_Candidate := True;

                  --  Hide the candidate because it is not used in a proper
                  --  context.

                  else
                     Candidate        := Empty;
                     Is_New_Candidate := False;
                  end if;
               end if;

            --  Ada 2005 (AI-217): Handle shadow entities associated with
            --  types declared in limited-withed nested packages. We don't need
            --  to handle E_Incomplete_Subtype entities because the entities
            --  in the limited view are always E_Incomplete_Type and
            --  E_Class_Wide_Type entities (see Build_Limited_Views).

            --  Regarding the expression used to evaluate the scope, it
            --  is important to note that the limited view also has shadow
            --  entities associated nested packages. For this reason the
            --  correct scope of the entity is the scope of the real entity.
            --  The non-limited view may itself be incomplete, in which case
            --  get the full view if available.

            elsif Ekind (Id) in E_Incomplete_Type | E_Class_Wide_Type
              and then From_Limited_With (Id)
              and then Present (Non_Limited_View (Id))
              and then Scope (Non_Limited_View (Id)) = P_Name
            then
               Candidate        := Get_Full_View (Non_Limited_View (Id));
               Is_New_Candidate := True;

            --  Handle special case where the prefix is a renaming of a shadow
            --  package which is visible. Required to avoid reporting spurious
            --  errors.

            elsif Ekind (P_Name) = E_Package
              and then From_Limited_With (P_Name)
              and then not From_Limited_With (Id)
              and then Sloc (Scope (Id)) = Sloc (P_Name)
              and then Unit_Is_Visible (Cunit (Get_Source_Unit (P_Name)))
            then
               Candidate        := Get_Full_View (Id);
               Is_New_Candidate := True;

            --  An unusual case arises with a fully qualified name for an
            --  entity local to a generic child unit package, within an
            --  instantiation of that package. The name of the unit now
            --  denotes the renaming created within the instance. This is
            --  only relevant in an instance body, see below.

            elsif Is_Generic_Instance (Scope (Id))
              and then In_Open_Scopes (Scope (Id))
              and then In_Instance_Body
              and then Ekind (Scope (Id)) = E_Package
              and then Ekind (Id) = E_Package
              and then Renamed_Entity (Id) = Scope (Id)
              and then Is_Immediately_Visible (P_Name)
            then
               Is_New_Candidate := True;

            else
               Is_New_Candidate := False;
            end if;

            if Is_New_Candidate then

               --  If entity is a child unit, either it is a visible child of
               --  the prefix, or we are in the body of a generic prefix, as
               --  will happen when a child unit is instantiated in the body
               --  of a generic parent. This is because the instance body does
               --  not restore the full compilation context, given that all
               --  non-local references have been captured.

               if Is_Child_Unit (Id) or else P_Name = Standard_Standard then
                  exit when Is_Visible_Lib_Unit (Id)
                    or else (Is_Child_Unit (Id)
                              and then In_Open_Scopes (Scope (Id))
                              and then In_Instance_Body);
               else
                  exit when not Is_Hidden (Id);
               end if;

               exit when Is_Immediately_Visible (Id);
            end if;

            Id := Homonym (Id);
         end loop;
      end;

      if No (Id)
        and then Ekind (P_Name) in E_Procedure | E_Function
        and then Is_Generic_Instance (P_Name)
      then
         --  Expanded name denotes entity in (instance of) generic subprogram.
         --  The entity may be in the subprogram instance, or may denote one of
         --  the formals, which is declared in the enclosing wrapper package.

         P_Name := Scope (P_Name);

         Id := Current_Entity (Selector);
         while Present (Id) loop
            exit when Scope (Id) = P_Name;
            Id := Homonym (Id);
         end loop;
      end if;

      if No (Id) or else Chars (Id) /= Chars (Selector) then
         Set_Etype (N, Any_Type);

         --  If we are looking for an entity defined in System, try to find it
         --  in the child package that may have been provided as an extension
         --  to System. The Extend_System pragma will have supplied the name of
         --  the extension, which may have to be loaded.

         if Chars (P_Name) = Name_System
           and then Scope (P_Name) = Standard_Standard
           and then Present (System_Extend_Unit)
           and then Present_System_Aux (N)
         then
            Set_Entity (Prefix (N), System_Aux_Id);
            Find_Expanded_Name (N);
            return;

         --  There is an implicit instance of the predefined operator in
         --  the given scope. The operator entity is defined in Standard.
         --  Has_Implicit_Operator makes the node into an Expanded_Name.

         elsif Nkind (Selector) = N_Operator_Symbol
           and then Has_Implicit_Operator (N)
         then
            return;

         --  If there is no literal defined in the scope denoted by the
         --  prefix, the literal may belong to (a type derived from)
         --  Standard_Character, for which we have no explicit literals.

         elsif Nkind (Selector) = N_Character_Literal
           and then Has_Implicit_Character_Literal (N)
         then
            return;

         else
            --  If the prefix is a single concurrent object, use its name in
            --  the error message, rather than that of the anonymous type.

            if Is_Concurrent_Type (P_Name)
              and then Is_Internal_Name (Chars (P_Name))
            then
               Error_Msg_Node_2 := Entity (Prefix (N));
            else
               Error_Msg_Node_2 := P_Name;
            end if;

            if P_Name = System_Aux_Id then
               P_Name := Scope (P_Name);
               Set_Entity (Prefix (N), P_Name);
            end if;

            if Present (Candidate) then

               --  If we know that the unit is a child unit we can give a more
               --  accurate error message.

               if Is_Child_Unit (Candidate) then

                  --  If the candidate is a private child unit and we are in
                  --  the visible part of a public unit, specialize the error
                  --  message. There might be a private with_clause for it,
                  --  but it is not currently active.

                  if Is_Private_Descendant (Candidate)
                    and then Ekind (Current_Scope) = E_Package
                    and then not In_Private_Part (Current_Scope)
                    and then not Is_Private_Descendant (Current_Scope)
                  then
                     Error_Msg_N
                       ("private child unit& is not visible here", Selector);

                  --  Normal case where we have a missing with for a child unit

                  else
                     Error_Msg_Qual_Level := 99;
                     Error_Msg_NE -- CODEFIX
                       ("missing `WITH &;`", Selector, Candidate);
                     Error_Msg_Qual_Level := 0;
                  end if;

                  --  Here we don't know that this is a child unit

               else
                  Error_Msg_NE ("& is not a visible entity of&", N, Selector);
               end if;

            else
               --  Within the instantiation of a child unit, the prefix may
               --  denote the parent instance, but the selector has the name
               --  of the original child. That is to say, when A.B appears
               --  within an instantiation of generic child unit B, the scope
               --  stack includes an instance of A (P_Name) and an instance
               --  of B under some other name. We scan the scope to find this
               --  child instance, which is the desired entity.
               --  Note that the parent may itself be a child instance, if
               --  the reference is of the form A.B.C, in which case A.B has
               --  already been rewritten with the proper entity.

               if In_Open_Scopes (P_Name)
                 and then Is_Generic_Instance (P_Name)
               then
                  declare
                     Gen_Par : constant Entity_Id :=
                                 Generic_Parent (Specification
                                   (Unit_Declaration_Node (P_Name)));
                     S : Entity_Id := Current_Scope;
                     P : Entity_Id;

                  begin
                     for J in reverse 0 .. Scope_Stack.Last loop
                        S := Scope_Stack.Table (J).Entity;

                        exit when S = Standard_Standard;

                        if Ekind (S) in E_Function | E_Package | E_Procedure
                        then
                           P :=
                             Generic_Parent (Specification
                               (Unit_Declaration_Node (S)));

                           --  Check that P is a generic child of the generic
                           --  parent of the prefix.

                           if Present (P)
                             and then Chars (P) = Chars (Selector)
                             and then Scope (P) = Gen_Par
                           then
                              Id := S;
                              goto Found;
                           end if;
                        end if;

                     end loop;
                  end;
               end if;

               --  If this is a selection from Ada, System or Interfaces, then
               --  we assume a missing with for the corresponding package.

               if Is_Known_Unit (N)
                 and then not (Present (Entity (Prefix (N)))
                                and then Scope (Entity (Prefix (N))) /=
                                           Standard_Standard)
               then
                  if not Error_Posted (N) then
                     Error_Msg_NE
                       ("& is not a visible entity of&", Prefix (N), Selector);
                     Error_Missing_With_Of_Known_Unit (Prefix (N));
                  end if;

               --  If this is a selection from a dummy package, then suppress
               --  the error message, of course the entity is missing if the
               --  package is missing.

               elsif Sloc (Error_Msg_Node_2) = No_Location then
                  null;

               --  Here we have the case of an undefined component

               else
                  --  The prefix may hide a homonym in the context that
                  --  declares the desired entity. This error can use a
                  --  specialized message.

                  if In_Open_Scopes (P_Name) then
                     declare
                        H : constant Entity_Id := Homonym (P_Name);

                     begin
                        if Present (H)
                          and then Is_Compilation_Unit (H)
                          and then
                            (Is_Immediately_Visible (H)
                              or else Is_Visible_Lib_Unit (H))
                        then
                           Id := First_Entity (H);
                           while Present (Id) loop
                              if Chars (Id) = Chars (Selector) then
                                 Error_Msg_Qual_Level := 99;
                                 Error_Msg_Name_1 := Chars (Selector);
                                 Error_Msg_NE
                                   ("% not declared in&", N, P_Name);
                                 Error_Msg_NE
                                   ("\use fully qualified name starting with "
                                    & "Standard to make& visible", N, H);
                                 Error_Msg_Qual_Level := 0;
                                 goto Done;
                              end if;

                              Next_Entity (Id);
                           end loop;
                        end if;

                        --  If not found, standard error message

                        Error_Msg_NE ("& not declared in&", N, Selector);

                        <<Done>> null;
                     end;

                  else
                     --  Might be worth specializing the case when the prefix
                     --  is a limited view.
                     --  ... not declared in limited view of...

                     Error_Msg_NE ("& not declared in&", N, Selector);
                  end if;

                  --  Check for misspelling of some entity in prefix

                  Id := First_Entity (P_Name);
                  while Present (Id) loop
                     if Is_Bad_Spelling_Of (Chars (Id), Chars (Selector))
                       and then not Is_Internal_Name (Chars (Id))
                     then
                        Error_Msg_NE -- CODEFIX
                          ("possible misspelling of&", Selector, Id);
                        exit;
                     end if;

                     Next_Entity (Id);
                  end loop;

                  --  Specialize the message if this may be an instantiation
                  --  of a child unit that was not mentioned in the context.

                  if Nkind (Parent (N)) = N_Package_Instantiation
                    and then Is_Generic_Instance (Entity (Prefix (N)))
                    and then Is_Compilation_Unit
                               (Generic_Parent (Parent (Entity (Prefix (N)))))
                  then
                     Error_Msg_Node_2 := Selector;
                     Error_Msg_N -- CODEFIX
                       ("\missing `WITH &.&;`", Prefix (N));
                  end if;
               end if;
            end if;

            Id := Any_Id;
         end if;
      end if;

      <<Found>>
      if Comes_From_Source (N)
        and then Is_Remote_Access_To_Subprogram_Type (Id)
        and then Ekind (Id) = E_Access_Subprogram_Type
        and then Present (Equivalent_Type (Id))
      then
         --  If we are not actually generating distribution code (i.e. the
         --  current PCS is the dummy non-distributed version), then the
         --  Equivalent_Type will be missing, and Id should be treated as
         --  a regular access-to-subprogram type.

         Id := Equivalent_Type (Id);
         Set_Chars (Selector, Chars (Id));
      end if;

      --  Ada 2005 (AI-50217): Check usage of entities in limited withed units

      if Ekind (P_Name) = E_Package and then From_Limited_With (P_Name) then
         if From_Limited_With (Id)
           or else Is_Type (Id)
           or else Ekind (Id) = E_Package
         then
            null;
         else
            Error_Msg_N
              ("limited withed package can only be used to access incomplete "
               & "types", N);
         end if;
      end if;

      if Is_Task_Type (P_Name)
        and then ((Ekind (Id) = E_Entry
                    and then Nkind (Parent (N)) /= N_Attribute_Reference)
                   or else
                     (Ekind (Id) = E_Entry_Family
                       and then
                         Nkind (Parent (Parent (N))) /= N_Attribute_Reference))
      then
         --  If both the task type and the entry are in scope, this may still
         --  be the expanded name of an entry formal.

         if In_Open_Scopes (Id)
           and then Nkind (Parent (N)) = N_Selected_Component
         then
            null;

         else
            --  It is an entry call after all, either to the current task
            --  (which will deadlock) or to an enclosing task.

            Analyze_Selected_Component (N);
            return;
         end if;
      end if;

      case Nkind (N) is
         when N_Selected_Component =>
            Reinit_Field_To_Zero (N, F_Is_Prefixed_Call);
            Change_Selected_Component_To_Expanded_Name (N);

         when N_Expanded_Name =>
            null;

         when others =>
            pragma Assert (False);
      end case;

      --  Preserve relevant elaboration-related attributes of the context which
      --  are no longer available or very expensive to recompute once analysis,
      --  resolution, and expansion are over.

      Mark_Elaboration_Attributes
        (N_Id     => N,
         Checks   => True,
         Modes    => True,
         Warnings => True);

      --  Set appropriate type

      if Is_Type (Id) then
         Set_Etype (N, Id);
      else
         Set_Etype (N, Get_Full_View (Etype (Id)));
      end if;

      --  Do style check and generate reference, but skip both steps if this
      --  entity has homonyms, since we may not have the right homonym set yet.
      --  The proper homonym will be set during the resolve phase.

      if Has_Homonym (Id) then
         Set_Entity (N, Id);

      else
         Set_Entity_Or_Discriminal (N, Id);

         case Known_To_Be_Assigned (N, Only_LHS => True) is
            when True =>
               Generate_Reference (Id, N, 'm');

            when False =>
               Generate_Reference (Id, N, 'r');

         end case;
      end if;

      --  Check for violation of No_Wide_Characters

      Check_Wide_Character_Restriction (Id, N);

      if Is_Self_Hidden (Id) then
         Premature_Usage (N);

      elsif Is_Overloadable (Id) and then Present (Homonym (Id)) then
         declare
            H : Entity_Id := Homonym (Id);

         begin
            while Present (H) loop
               if Scope (H) = Scope (Id)
                 and then (not Is_Hidden (H)
                            or else Is_Immediately_Visible (H))
               then
                  Collect_Interps (N);
                  exit;
               end if;

               H := Homonym (H);
            end loop;

            --  If an extension of System is present, collect possible explicit
            --  overloadings declared in the extension.

            if Chars (P_Name) = Name_System
              and then Scope (P_Name) = Standard_Standard
              and then Present (System_Extend_Unit)
              and then Present_System_Aux (N)
            then
               H := Current_Entity (Id);

               while Present (H) loop
                  if Scope (H) = System_Aux_Id then
                     Add_One_Interp (N, H, Etype (H));
                  end if;

                  H := Homonym (H);
               end loop;
            end if;
         end;
      end if;

      if Nkind (Selector_Name (N)) = N_Operator_Symbol
        and then Scope (Id) /= Standard_Standard
      then
         --  In addition to user-defined operators in the given scope, there
         --  may be an implicit instance of the predefined operator. The
         --  operator (defined in Standard) is found in Has_Implicit_Operator,
         --  and added to the interpretations. Procedure Add_One_Interp will
         --  determine which hides which.

         if Has_Implicit_Operator (N) then
            null;
         end if;
      end if;

      --  If there is a single interpretation for N we can generate a
      --  reference to the unique entity found.

      if Is_Overloadable (Id) and then not Is_Overloaded (N) then
         Generate_Reference (Id, N);
      end if;

      --  Mark relevant use-type and use-package clauses as effective if the
      --  node in question is not overloaded and therefore does not require
      --  resolution.

      if Nkind (N) not in N_Subexpr or else not Is_Overloaded (N) then
         Mark_Use_Clauses (N);
      end if;

      Check_Restriction_No_Use_Of_Entity (N);

      --  Annotate the tree by creating a variable reference marker in case the
      --  original variable reference is folded or optimized away. The variable
      --  reference marker is automatically saved for later examination by the
      --  ABE Processing phase. Variable references which act as actuals in a
      --  call require special processing and are left to Resolve_Actuals. The
      --  reference is a write when it appears on the left hand side of an
      --  assignment.

      if Needs_Variable_Reference_Marker
           (N        => N,
            Calls_OK => False)
      then
         declare
            Is_Assignment_LHS : constant Boolean := Known_To_Be_Assigned (N);

         begin
            Build_Variable_Reference_Marker
              (N     => N,
               Read  => not Is_Assignment_LHS,
               Write => Is_Assignment_LHS);
         end;
      end if;
   end Find_Expanded_Name;

   --------------------
   -- Find_First_Use --
   --------------------

   function Find_First_Use (Use_Clause : Node_Id) return Node_Id is
      Curr : Node_Id;

   begin
      --  Loop through the Prev_Use_Clause chain

      Curr := Use_Clause;
      while Present (Prev_Use_Clause (Curr)) loop
         Curr := Prev_Use_Clause (Curr);
      end loop;

      return Curr;
   end Find_First_Use;

   -------------------------
   -- Find_Renamed_Entity --
   -------------------------

   function Find_Renamed_Entity
     (N         : Node_Id;
      Nam       : Node_Id;
      New_S     : Entity_Id;
      Is_Actual : Boolean := False) return Entity_Id
   is
      Ind   : Interp_Index;
      I1    : Interp_Index := 0; -- Suppress junk warnings
      It    : Interp;
      It1   : Interp;
      Old_S : Entity_Id;
      Inst  : Entity_Id;

      function Find_Nearer_Entity
        (New_S  : Entity_Id;
         Old1_S : Entity_Id;
         Old2_S : Entity_Id) return Entity_Id;
      --  Determine whether one of Old_S1 and Old_S2 is nearer to New_S than
      --  the other, and return it if so. Return Empty otherwise. We use this
      --  in conjunction with Inherit_Renamed_Profile to simplify later type
      --  disambiguation for actual subprograms in instances.

      function Is_Visible_Operation (Op : Entity_Id) return Boolean;
      --  If the renamed entity is an implicit operator, check whether it is
      --  visible because its operand type is properly visible. This check
      --  applies to explicit renamed entities that appear in the source in a
      --  renaming declaration or a formal subprogram instance, but not to
      --  default generic actuals with a name.

      function Report_Overload return Entity_Id;
      --  List possible interpretations, and specialize message in the
      --  case of a generic actual.

      function Within (Inner, Outer : Entity_Id) return Boolean;
      --  Determine whether a candidate subprogram is defined within the
      --  enclosing instance. If yes, it has precedence over outer candidates.

      --------------------------
      --  Find_Nearer_Entity  --
      --------------------------

      function Find_Nearer_Entity
        (New_S  : Entity_Id;
         Old1_S : Entity_Id;
         Old2_S : Entity_Id) return Entity_Id
      is
         New_F  : Entity_Id;
         Old1_F : Entity_Id;
         Old2_F : Entity_Id;
         Anc_T  : Entity_Id;

      begin
         New_F  := First_Formal (New_S);
         Old1_F := First_Formal (Old1_S);
         Old2_F := First_Formal (Old2_S);

         --  The criterion is whether the type of the formals of one of Old1_S
         --  and Old2_S is an ancestor subtype of the type of the corresponding
         --  formals of New_S while the other is not (we already know that they
         --  are all subtypes of the same base type).

         --  This makes it possible to find the more correct renamed entity in
         --  the case of a generic instantiation nested in an enclosing one for
         --  which different formal types get the same actual type, which will
         --  in turn make it possible for Inherit_Renamed_Profile to preserve
         --  types on formal parameters and ultimately simplify disambiguation.

         --  Consider the follow package G:

         --    generic
         --       type Item_T is private;
         --       with function Compare (L, R: Item_T) return Boolean is <>;

         --       type Bound_T is private;
         --       with function Compare (L, R : Bound_T) return Boolean is <>;
         --    package G is
         --       ...
         --    end G;

         --    package body G is
         --       package My_Inner is Inner_G (Bound_T);
         --       ...
         --    end G;

         --    with the following package Inner_G:

         --    generic
         --       type T is private;
         --       with function Compare (L, R: T) return Boolean is <>;
         --    package Inner_G is
         --       function "<" (L, R: T) return Boolean is (Compare (L, R));
         --    end Inner_G;

         --  If G is instantiated on the same actual type with a single Compare
         --  function:

         --    type T is ...
         --    function Compare (L, R : T) return Boolean;
         --    package My_G is new (T, T);

         --  then the renaming generated for Compare in the inner instantiation
         --  is ambiguous: it can rename either of the renamings generated for
         --  the outer instantiation. Now if the first one is picked up, then
         --  the subtypes of the formal parameters of the renaming will not be
         --  preserved in Inherit_Renamed_Profile because they are subtypes of
         --  the Bound_T formal type and not of the Item_T formal type, so we
         --  need to arrange for the second one to be picked up instead.

         while Present (New_F) loop
            if Etype (Old1_F) /= Etype (Old2_F) then
               Anc_T := Ancestor_Subtype (Etype (New_F));

               if Etype (Old1_F) = Anc_T then
                  return Old1_S;
               elsif Etype (Old2_F) = Anc_T then
                  return Old2_S;
               end if;
            end if;

            Next_Formal (New_F);
            Next_Formal (Old1_F);
            Next_Formal (Old2_F);
         end loop;

         pragma Assert (No (Old1_F));
         pragma Assert (No (Old2_F));

         return Empty;
      end Find_Nearer_Entity;

      --------------------------
      -- Is_Visible_Operation --
      --------------------------

      function Is_Visible_Operation (Op : Entity_Id) return Boolean is
         Scop : Entity_Id;
         Typ  : Entity_Id;
         Btyp : Entity_Id;

      begin
         if Ekind (Op) /= E_Operator
           or else Scope (Op) /= Standard_Standard
           or else (In_Instance
                     and then (not Is_Actual
                                or else Present (Enclosing_Instance)))
         then
            return True;

         else
            --  For a fixed point type operator, check the resulting type,
            --  because it may be a mixed mode integer * fixed operation.

            if Present (Next_Formal (First_Formal (New_S)))
              and then Is_Fixed_Point_Type (Etype (New_S))
            then
               Typ := Etype (New_S);
            else
               Typ := Etype (First_Formal (New_S));
            end if;

            Btyp := Base_Type (Typ);

            if Nkind (Nam) /= N_Expanded_Name then
               return (In_Open_Scopes (Scope (Btyp))
                        or else Is_Potentially_Use_Visible (Btyp)
                        or else In_Use (Btyp)
                        or else In_Use (Scope (Btyp)));

            else
               Scop := Entity (Prefix (Nam));

               if Ekind (Scop) = E_Package
                 and then Present (Renamed_Entity (Scop))
               then
                  Scop := Renamed_Entity (Scop);
               end if;

               --  Operator is visible if prefix of expanded name denotes
               --  scope of type, or else type is defined in System_Aux
               --  and the prefix denotes System.

               return Scope (Btyp) = Scop
                 or else (Scope (Btyp) = System_Aux_Id
                           and then Scope (Scope (Btyp)) = Scop);
            end if;
         end if;
      end Is_Visible_Operation;

      ------------
      -- Within --
      ------------

      function Within (Inner, Outer : Entity_Id) return Boolean is
         Sc : Entity_Id;

      begin
         Sc := Scope (Inner);
         while Sc /= Standard_Standard loop
            if Sc = Outer then
               return True;
            else
               Sc := Scope (Sc);
            end if;
         end loop;

         return False;
      end Within;

      ---------------------
      -- Report_Overload --
      ---------------------

      function Report_Overload return Entity_Id is
      begin
         if Is_Actual then
            Error_Msg_NE -- CODEFIX
              ("ambiguous actual subprogram&, " &
                 "possible interpretations:", N, Nam);
         else
            Error_Msg_N -- CODEFIX
              ("ambiguous subprogram, " &
                 "possible interpretations:", N);
         end if;

         List_Interps (Nam, N);
         return Old_S;
      end Report_Overload;

   --  Start of processing for Find_Renamed_Entity

   begin
      Old_S := Any_Id;
      Candidate_Renaming := Empty;

      if Is_Overloaded (Nam) then
         Get_First_Interp (Nam, Ind, It);
         while Present (It.Nam) loop
            if Entity_Matches_Spec (It.Nam, New_S)
              and then Is_Visible_Operation (It.Nam)
            then
               if Old_S /= Any_Id then

                  --  Note: The call to Disambiguate only happens if a
                  --  previous interpretation was found, in which case I1
                  --  has received a value.

                  It1 := Disambiguate (Nam, I1, Ind, Etype (Old_S));

                  if It1 = No_Interp then
                     Inst := Enclosing_Instance;

                     if Present (Inst) then
                        if Within (It.Nam, Inst) then
                           if Within (Old_S, Inst) then
                              declare
                                 It_D  : constant Uint :=
                                   Scope_Depth_Default_0 (It.Nam);
                                 Old_D : constant Uint :=
                                   Scope_Depth_Default_0 (Old_S);
                                 N_Ent : Entity_Id;
                              begin
                                 --  Choose the innermost subprogram, which
                                 --  would hide the outer one in the generic.

                                 if Old_D > It_D then
                                    return Old_S;
                                 elsif It_D > Old_D then
                                    return It.Nam;
                                 end if;

                                 --  Otherwise, if we can determine that one
                                 --  of the entities is nearer to the renaming
                                 --  than the other, choose it. If not, then
                                 --  return the newer one as done historically.

                                 N_Ent :=
                                     Find_Nearer_Entity (New_S, Old_S, It.Nam);
                                 if Present (N_Ent) then
                                    return N_Ent;
                                 else
                                    return It.Nam;
                                 end if;
                              end;
                           end if;

                        elsif Within (Old_S, Inst) then
                           return Old_S;

                        else
                           return Report_Overload;
                        end if;

                     --  If not within an instance, ambiguity is real

                     else
                        return Report_Overload;
                     end if;

                  else
                     Old_S := It1.Nam;
                     exit;
                  end if;

               else
                  I1 := Ind;
                  Old_S := It.Nam;
               end if;

            elsif
              Present (First_Formal (It.Nam))
                and then Present (First_Formal (New_S))
                and then Base_Type (Etype (First_Formal (It.Nam))) =
                         Base_Type (Etype (First_Formal (New_S)))
            then
               Candidate_Renaming := It.Nam;
            end if;

            Get_Next_Interp (Ind, It);
         end loop;

         Set_Entity (Nam, Old_S);

         if Old_S /= Any_Id then
            Set_Is_Overloaded (Nam, False);
         end if;

      --  Non-overloaded case

      else
         if Is_Actual
           and then Present (Enclosing_Instance)
           and then Entity_Matches_Spec (Entity (Nam), New_S)
         then
            Old_S := Entity (Nam);

         elsif Entity_Matches_Spec (Entity (Nam), New_S) then
            Candidate_Renaming := New_S;

            if Is_Visible_Operation (Entity (Nam)) then
               Old_S := Entity (Nam);
            end if;

         elsif Present (First_Formal (Entity (Nam)))
           and then Present (First_Formal (New_S))
           and then Base_Type (Etype (First_Formal (Entity (Nam)))) =
                    Base_Type (Etype (First_Formal (New_S)))
         then
            Candidate_Renaming := Entity (Nam);
         end if;
      end if;

      return Old_S;
   end Find_Renamed_Entity;

   -----------------------------
   -- Find_Selected_Component --
   -----------------------------

   procedure Find_Selected_Component (N : Node_Id) is
      P : constant Node_Id := Prefix (N);

      P_Name : Entity_Id;
      --  Entity denoted by prefix

      P_Type : Entity_Id;
      --  and its type

      Nam : Node_Id;

      function Available_Subtype return Boolean;
      --  A small optimization: if the prefix is constrained and the component
      --  is an array type we may already have a usable subtype for it, so we
      --  can use it rather than generating a new one, because the bounds
      --  will be the values of the discriminants and not discriminant refs.
      --  This simplifies value tracing in GNATprove. For consistency, both
      --  the entity name and the subtype come from the constrained component.

      --  This is only used in GNATprove mode: when generating code it may be
      --  necessary to create an itype in the scope of use of the selected
      --  component, e.g. in the context of a expanded record equality.

      function Is_Reference_In_Subunit return Boolean;
      --  In a subunit, the scope depth is not a proper measure of hiding,
      --  because the context of the proper body may itself hide entities in
      --  parent units. This rare case requires inspecting the tree directly
      --  because the proper body is inserted in the main unit and its context
      --  is simply added to that of the parent.

      -----------------------
      -- Available_Subtype --
      -----------------------

      function Available_Subtype return Boolean is
         Comp : Entity_Id;

      begin
         if GNATprove_Mode then
            Comp := First_Entity (Etype (P));
            while Present (Comp) loop
               if Chars (Comp) = Chars (Selector_Name (N)) then
                  Set_Etype  (N, Etype (Comp));
                  Set_Entity (Selector_Name (N), Comp);
                  Set_Etype  (Selector_Name (N), Etype (Comp));
                  return True;
               end if;

               Next_Component (Comp);
            end loop;
         end if;

         return False;
      end Available_Subtype;

      -----------------------------
      -- Is_Reference_In_Subunit --
      -----------------------------

      function Is_Reference_In_Subunit return Boolean is
         Clause    : Node_Id;
         Comp_Unit : Node_Id;

      begin
         Comp_Unit := N;
         while Present (Comp_Unit)
           and then Nkind (Comp_Unit) /= N_Compilation_Unit
         loop
            Comp_Unit := Parent (Comp_Unit);
         end loop;

         if No (Comp_Unit) or else Nkind (Unit (Comp_Unit)) /= N_Subunit then
            return False;
         end if;

         --  Now check whether the package is in the context of the subunit

         Clause := First (Context_Items (Comp_Unit));
         while Present (Clause) loop
            if Nkind (Clause) = N_With_Clause
              and then Entity (Name (Clause)) = P_Name
            then
               return True;
            end if;

            Next (Clause);
         end loop;

         return False;
      end Is_Reference_In_Subunit;

   --  Start of processing for Find_Selected_Component

   begin
      Analyze (P);

      if Nkind (P) = N_Error then
         return;
      end if;

      --  If the selector already has an entity, the node has been constructed
      --  in the course of expansion, and is known to be valid. Do not verify
      --  that it is defined for the type (it may be a private component used
      --  in the expansion of record equality).

      if Present (Entity (Selector_Name (N))) then
         if No (Etype (N)) or else Etype (N) = Any_Type then
            declare
               Sel_Name : constant Node_Id   := Selector_Name (N);
               Selector : constant Entity_Id := Entity (Sel_Name);
               C_Etype  : Node_Id;

            begin
               Set_Etype (Sel_Name, Etype (Selector));

               if not Is_Entity_Name (P) then
                  Resolve (P);
               end if;

               --  Build an actual subtype except for the first parameter
               --  of an init proc, where this actual subtype is by
               --  definition incorrect, since the object is uninitialized
               --  (and does not even have defined discriminants etc.)

               if Is_Entity_Name (P)
                 and then Ekind (Entity (P)) = E_Function
               then
                  Nam := New_Copy (P);

                  if Is_Overloaded (P) then
                     Save_Interps (P, Nam);
                  end if;

                  Rewrite (P, Make_Function_Call (Sloc (P), Name => Nam));
                  Analyze_Call (P);
                  Analyze_Selected_Component (N);
                  return;

               elsif Ekind (Selector) = E_Component
                 and then (not Is_Entity_Name (P)
                            or else Chars (Entity (P)) /= Name_uInit)
               then
                  --  Check if we already have an available subtype we can use

                  if Ekind (Etype (P)) = E_Record_Subtype
                    and then Nkind (Parent (Etype (P))) = N_Subtype_Declaration
                    and then Is_Array_Type (Etype (Selector))
                    and then not Is_Packed (Etype (Selector))
                    and then Available_Subtype
                  then
                     return;

                  --  Do not build the subtype when referencing components of
                  --  dispatch table wrappers. Required to avoid generating
                  --  elaboration code with HI runtimes.

                  elsif Is_RTE (Scope (Selector), RE_Dispatch_Table_Wrapper)
                    or else
                      Is_RTE (Scope (Selector), RE_No_Dispatch_Table_Wrapper)
                  then
                     C_Etype := Empty;
                  else
                     C_Etype :=
                       Build_Actual_Subtype_Of_Component
                         (Etype (Selector), N);
                  end if;

               else
                  C_Etype := Empty;
               end if;

               if No (C_Etype) then
                  C_Etype := Etype (Selector);
               else
                  Insert_Action (N, C_Etype);
                  C_Etype := Defining_Identifier (C_Etype);
               end if;

               Set_Etype (N, C_Etype);
            end;

         --  If the selected component appears within a default expression
         --  and it has an actual subtype, the preanalysis has not yet
         --  completed its analysis, because Insert_Actions is disabled in
         --  that context. Within the init proc of the enclosing type we
         --  must complete this analysis, if an actual subtype was created.

         elsif Inside_Init_Proc then
            declare
               Typ  : constant Entity_Id := Etype (N);
               Decl : constant Node_Id   := Declaration_Node (Typ);
            begin
               if Nkind (Decl) = N_Subtype_Declaration
                 and then not Analyzed (Decl)
                 and then Is_List_Member (Decl)
                 and then No (Parent (Decl))
               then
                  Remove (Decl);
                  Insert_Action (N, Decl);
               end if;
            end;
         end if;

         return;

      elsif Is_Entity_Name (P) then
         P_Name := Entity (P);

         --  The prefix may denote an enclosing type which is the completion
         --  of an incomplete type declaration.

         if Is_Type (P_Name) then
            Set_Entity (P, Get_Full_View (P_Name));
            Set_Etype  (P, Entity (P));
            P_Name := Entity (P);
         end if;

         P_Type := Base_Type (Etype (P));

         if Debug_Flag_E then
            Write_Str ("Found prefix type to be ");
            Write_Entity_Info (P_Type, "      "); Write_Eol;
         end if;

         --  If the prefix's type is an access type, get to the record type

         if Is_Access_Type (P_Type) then
            P_Type := Implicitly_Designated_Type (P_Type);
         end if;

         --  First check for components of a record object (not the result of
         --  a call, which is handled below). This also covers the case where
         --  the extension feature that supports the prefixed form of calls
         --  for primitives of untagged types is enabled (excluding concurrent
         --  cases, which are handled further below).

         if Is_Type (P_Type)
           and then (Has_Components (P_Type)
                      or else (Core_Extensions_Allowed
                                and then not Is_Concurrent_Type (P_Type)))
           and then not Is_Overloadable (P_Name)
           and then not Is_Type (P_Name)
         then
            --  Selected component of record. Type checking will validate
            --  name of selector.

            --  ??? Could we rewrite an implicit dereference into an explicit
            --  one here?

            Analyze_Selected_Component (N);

         --  Reference to type name in predicate/invariant expression

         elsif Is_Concurrent_Type (P_Type)
           and then not In_Open_Scopes (P_Name)
           and then (not Is_Concurrent_Type (Etype (P_Name))
                      or else not In_Open_Scopes (Etype (P_Name)))
         then
            --  Call to protected operation or entry. Type checking is
            --  needed on the prefix.

            Analyze_Selected_Component (N);

         elsif (In_Open_Scopes (P_Name)
                 and then Ekind (P_Name) /= E_Void
                 and then not Is_Overloadable (P_Name))
           or else (Is_Concurrent_Type (Etype (P_Name))
                     and then In_Open_Scopes (Etype (P_Name)))
         then
            --  Prefix denotes an enclosing loop, block, or task, i.e. an
            --  enclosing construct that is not a subprogram or accept.

            --  A special case: a protected body may call an operation
            --  on an external object of the same type, in which case it
            --  is not an expanded name. If the prefix is the type itself,
            --  or the context is a single synchronized object it can only
            --  be interpreted as an expanded name.

            if Is_Concurrent_Type (Etype (P_Name)) then
               if Is_Type (P_Name)
                  or else Present (Anonymous_Object (Etype (P_Name)))
               then
                  Find_Expanded_Name (N);

               else
                  Analyze_Selected_Component (N);
                  return;
               end if;

            else
               Find_Expanded_Name (N);
            end if;

         elsif Ekind (P_Name) = E_Package then
            Find_Expanded_Name (N);

         elsif Is_Overloadable (P_Name) then

            --  The subprogram may be a renaming (of an enclosing scope) as
            --  in the case of the name of the generic within an instantiation.

            if Ekind (P_Name) in E_Procedure | E_Function
              and then Present (Alias (P_Name))
              and then Is_Generic_Instance (Alias (P_Name))
            then
               P_Name := Alias (P_Name);
            end if;

            if Is_Overloaded (P) then

               --  The prefix must resolve to a unique enclosing construct

               declare
                  Found : Boolean := False;
                  Ind   : Interp_Index;
                  It    : Interp;

               begin
                  Get_First_Interp (P, Ind, It);
                  while Present (It.Nam) loop
                     if In_Open_Scopes (It.Nam) then
                        if Found then
                           Error_Msg_N (
                              "prefix must be unique enclosing scope", N);
                           Set_Entity (N, Any_Id);
                           Set_Etype  (N, Any_Type);
                           return;

                        else
                           Found := True;
                           P_Name := It.Nam;
                        end if;
                     end if;

                     Get_Next_Interp (Ind, It);
                  end loop;
               end;
            end if;

            if In_Open_Scopes (P_Name) then
               Set_Entity (P, P_Name);
               Set_Is_Overloaded (P, False);
               Find_Expanded_Name (N);

            else
               --  If no interpretation as an expanded name is possible, it
               --  must be a selected component of a record returned by a
               --  function call. Reformat prefix as a function call, the rest
               --  is done by type resolution.

               --  Error if the prefix is procedure or entry, as is P.X

               if Ekind (P_Name) /= E_Function
                 and then
                   (not Is_Overloaded (P)
                     or else Nkind (Parent (N)) = N_Procedure_Call_Statement)
               then
                  --  Prefix may mention a package that is hidden by a local
                  --  declaration: let the user know. Scan the full homonym
                  --  chain, the candidate package may be anywhere on it.

                  if Present (Homonym (Current_Entity (P_Name))) then
                     P_Name := Current_Entity (P_Name);

                     while Present (P_Name) loop
                        exit when Ekind (P_Name) = E_Package;
                        P_Name := Homonym (P_Name);
                     end loop;

                     if Present (P_Name) then
                        if not Is_Reference_In_Subunit then
                           Error_Msg_Sloc := Sloc (Entity (Prefix (N)));
                           Error_Msg_NE
                             ("package& is hidden by declaration#", N, P_Name);
                        end if;

                        Set_Entity (Prefix (N), P_Name);
                        Find_Expanded_Name (N);
                        return;

                     else
                        P_Name := Entity (Prefix (N));
                     end if;
                  end if;

                  Error_Msg_NE
                    ("invalid prefix in selected component&", N, P_Name);
                  Change_Selected_Component_To_Expanded_Name (N);
                  Set_Entity (N, Any_Id);
                  Set_Etype (N, Any_Type);

               --  Here we have a function call, so do the reformatting

               else
                  Nam := New_Copy (P);
                  Save_Interps (P, Nam);

                  --  We use Replace here because this is one of those cases
                  --  where the parser has missclassified the node, and we fix
                  --  things up and then do the semantic analysis on the fixed
                  --  up node. Normally we do this using one of the Sinfo.CN
                  --  routines, but this is too tricky for that.

                  --  Note that using Rewrite would be wrong, because we would
                  --  have a tree where the original node is unanalyzed.

                  Replace (P,
                    Make_Function_Call (Sloc (P), Name => Nam));

                  --  Now analyze the reformatted node

                  Analyze_Call (P);

                  --  If the prefix is illegal after this transformation, there
                  --  may be visibility errors on the prefix. The safest is to
                  --  treat the selected component as an error.

                  if Error_Posted (P) then
                     Set_Etype (N, Any_Type);
                     return;

                  else
                     Analyze_Selected_Component (N);
                  end if;
               end if;
            end if;

         --  Remaining cases generate various error messages

         else
            --  Format node as expanded name, to avoid cascaded errors

            Change_Selected_Component_To_Expanded_Name (N);
            Set_Entity (N, Any_Id);
            Set_Etype  (N, Any_Type);

            --  Issue error message, but avoid this if error issued already.
            --  Use identifier of prefix if one is available.

            if P_Name = Any_Id then
               null;

            --  It is not an error if the prefix is the current instance of
            --  type name, e.g. the expression of a type aspect, when it is
            --  analyzed within a generic unit. We still have to verify that
            --  a component of that name exists, and decorate the node
            --  accordingly.

            elsif Inside_A_Generic
              and then Is_Entity_Name (P)
              and then Is_Current_Instance (P)
            then
               declare
                  Comp : Entity_Id;

               begin
                  Comp := First_Entity (Entity (P));
                  while Present (Comp) loop
                     if Chars (Comp) = Chars (Selector_Name (N)) then
                        Set_Entity (N, Comp);
                        Set_Etype  (N, Etype (Comp));
                        Set_Entity (Selector_Name (N), Comp);
                        Set_Etype  (Selector_Name (N), Etype (Comp));
                        return;
                     end if;

                     Next_Entity (Comp);
                  end loop;
               end;

            elsif Is_Self_Hidden (P_Name) then
               Premature_Usage (P);

            elsif Ekind (P_Name) = E_Generic_Package then
               Error_Msg_N ("prefix must not be a generic package", N);
               Error_Msg_N ("\use package instantiation as prefix instead", N);

            elsif Nkind (P) /= N_Attribute_Reference then

               --  This may have been meant as a prefixed call to a primitive
               --  of an untagged type. If it is a function call check type of
               --  its first formal and add explanation.

               declare
                  F : constant Entity_Id :=
                        Current_Entity (Selector_Name (N));
               begin
                  if Present (F)
                    and then Is_Subprogram (F)
                    and then Present (First_Entity (F))
                    and then not Is_Tagged_Type (Etype (First_Entity (F)))
                  then
                     Error_Msg_N
                       ("prefixed call is only allowed for objects of a "
                        & "tagged type unless -gnatX is used", N);

                     if not Core_Extensions_Allowed
                       and then
                         Try_Object_Operation (N, Allow_Extensions => True)
                     then
                        Error_Msg_N
                          ("\using -gnatX would make the prefixed call legal",
                           N);
                     end if;
                  end if;
               end;

               Error_Msg_N ("invalid prefix in selected component&", P);

               if Is_Incomplete_Type (P_Type)
                 and then Is_Access_Type (Etype (P))
               then
                  Error_Msg_N
                    ("\dereference must not be of an incomplete type "
                     & "(RM 3.10.1)", P);
               end if;

            else
               Error_Msg_N ("invalid prefix in selected component", P);
            end if;
         end if;
      else
         --  If prefix is not the name of an entity, it must be an expression,
         --  whose type is appropriate for a record. This is determined by
         --  type resolution.

         Analyze_Selected_Component (N);
      end if;

      Analyze_Dimension (N);
   end Find_Selected_Component;

   ---------------
   -- Find_Type --
   ---------------

   procedure Find_Type (N : Node_Id) is
      C      : Entity_Id;
      Typ    : Entity_Id;
      T      : Entity_Id;
      T_Name : Entity_Id;

   begin
      if N = Error then
         return;

      elsif Nkind (N) = N_Attribute_Reference then

         --  Class attribute. This is not valid in Ada 83 mode, but we do not
         --  need to enforce that at this point, since the declaration of the
         --  tagged type in the prefix would have been flagged already.

         if Attribute_Name (N) = Name_Class then
            Check_Restriction (No_Dispatch, N);
            Find_Type (Prefix (N));

            --  Propagate error from bad prefix

            if Etype (Prefix (N)) = Any_Type then
               Set_Entity (N, Any_Type);
               Set_Etype  (N, Any_Type);
               return;
            end if;

            T := Base_Type (Entity (Prefix (N)));

            --  Case where type is not known to be tagged. Its appearance in
            --  the prefix of the 'Class attribute indicates that the full view
            --  will be tagged.

            if not Is_Tagged_Type (T) then
               if Ekind (T) = E_Incomplete_Type then

                  --  It is legal to denote the class type of an incomplete
                  --  type. The full type will have to be tagged, of course.
                  --  In Ada 2005 this usage is declared obsolescent, so we
                  --  warn accordingly. This usage is only legal if the type
                  --  is completed in the current scope, and not for a limited
                  --  view of a type.

                  if Ada_Version >= Ada_2005 then

                     --  Test whether the Available_View of a limited type view
                     --  is tagged, since the limited view may not be marked as
                     --  tagged if the type itself has an untagged incomplete
                     --  type view in its package.

                     if From_Limited_With (T)
                       and then not Is_Tagged_Type (Available_View (T))
                     then
                        Error_Msg_N
                          ("prefix of Class attribute must be tagged", N);
                        Set_Etype (N, Any_Type);
                        Set_Entity (N, Any_Type);
                        return;

                     else
                        if Restriction_Check_Required (No_Obsolescent_Features)
                        then
                           Check_Restriction
                             (No_Obsolescent_Features, Prefix (N));
                        end if;

                        if Warn_On_Obsolescent_Feature then
                           Error_Msg_N
                             ("applying ''Class to an untagged incomplete type"
                              & " is an obsolescent feature (RM J.11)?r?", N);
                        end if;
                     end if;
                  end if;

                  Set_Is_Tagged_Type (T);
                  Set_Direct_Primitive_Operations (T, New_Elmt_List);
                  Make_Class_Wide_Type (T);
                  Set_Entity (N, Class_Wide_Type (T));
                  Set_Etype  (N, Class_Wide_Type (T));

               elsif Ekind (T) = E_Private_Type
                 and then not Is_Generic_Type (T)
                 and then In_Private_Part (Scope (T))
               then
                  --  The Class attribute can be applied to an untagged private
                  --  type fulfilled by a tagged type prior to the full type
                  --  declaration (but only within the parent package's private
                  --  part). Create the class-wide type now and check that the
                  --  full type is tagged later during its analysis. Note that
                  --  we do not mark the private type as tagged, unlike the
                  --  case of incomplete types, because the type must still
                  --  appear untagged to outside units.

                  if No (Class_Wide_Type (T)) then
                     Make_Class_Wide_Type (T);
                  end if;

                  Set_Entity (N, Class_Wide_Type (T));
                  Set_Etype  (N, Class_Wide_Type (T));

               else
                  --  Should we introduce a type Any_Tagged and use Wrong_Type
                  --  here, it would be a bit more consistent???

                  Error_Msg_NE
                    ("tagged type required, found}",
                     Prefix (N), First_Subtype (T));
                  Set_Entity (N, Any_Type);
                  return;
               end if;

            --  Case of tagged type

            else
               if Is_Concurrent_Type (T) then
                  if No (Corresponding_Record_Type (Entity (Prefix (N)))) then

                     --  Previous error. Create a class-wide type for the
                     --  synchronized type itself, with minimal semantic
                     --  attributes, to catch other errors in some ACATS tests.

                     pragma Assert (Serious_Errors_Detected /= 0);
                     Make_Class_Wide_Type (T);
                     C := Class_Wide_Type (T);
                     Set_First_Entity (C, First_Entity (T));

                  else
                     C := Class_Wide_Type
                            (Corresponding_Record_Type (Entity (Prefix (N))));
                  end if;

               else
                  C := Class_Wide_Type (Entity (Prefix (N)));
               end if;

               Set_Entity_With_Checks (N, C);
               Generate_Reference (C, N);
               Set_Etype (N, C);
            end if;

         --  Base attribute, not allowed in Ada 83

         elsif Attribute_Name (N) = Name_Base then
            if Ada_Version = Ada_83 and then Comes_From_Source (N) then
               Error_Msg_N
                 ("(Ada 83) Base attribute not allowed in subtype mark", N);

            else
               Find_Type (Prefix (N));
               Typ := Entity (Prefix (N));

               if Ada_Version >= Ada_95
                 and then not Is_Scalar_Type (Typ)
                 and then not Is_Generic_Type (Typ)
               then
                  Error_Msg_N
                    ("prefix of Base attribute must be scalar type",
                      Prefix (N));

               elsif Warn_On_Redundant_Constructs
                 and then Base_Type (Typ) = Typ
               then
                  Error_Msg_NE -- CODEFIX
                    ("redundant attribute, & is its own base type?r?", N, Typ);
               end if;

               T := Base_Type (Typ);

               --  Rewrite attribute reference with type itself (see similar
               --  processing in Analyze_Attribute, case Base). Preserve prefix
               --  if present, for other legality checks.

               if Nkind (Prefix (N)) = N_Expanded_Name then
                  Rewrite (N,
                     Make_Expanded_Name (Sloc (N),
                       Chars         => Chars (T),
                       Prefix        => New_Copy (Prefix (Prefix (N))),
                       Selector_Name => New_Occurrence_Of (T, Sloc (N))));

               else
                  Rewrite (N, New_Occurrence_Of (T, Sloc (N)));
               end if;

               Set_Entity (N, T);
               Set_Etype (N, T);
            end if;

         elsif Attribute_Name (N) = Name_Stub_Type then

            --  This is handled in Analyze_Attribute

            Analyze (N);

         --  All other attributes are invalid in a subtype mark

         else
            Error_Msg_N ("invalid attribute in subtype mark", N);
         end if;

      else
         Analyze (N);

         if Is_Entity_Name (N) then
            T_Name := Entity (N);
         else
            Error_Msg_N ("subtype mark required in this context", N);
            Set_Etype (N, Any_Type);
            return;
         end if;

         if T_Name  = Any_Id or else Etype (N) = Any_Type then

            --  Undefined id. Make it into a valid type

            Set_Entity (N, Any_Type);

         elsif not Is_Type (T_Name)
           and then T_Name /= Standard_Void_Type
         then
            Error_Msg_Sloc := Sloc (T_Name);
            Error_Msg_N ("subtype mark required in this context", N);
            Error_Msg_NE ("\\found & declared#", N, T_Name);
            Set_Entity (N, Any_Type);

         elsif Is_Current_Instance (N) and then Comes_From_Source (N) then
            if Nkind (Parent (T_Name)) = N_Subtype_Declaration then
               Error_Msg_N ("reference to current instance of subtype" &
                            " does not denote a subtype (RM 8.6)", N);
            else
               Error_Msg_N ("reference to current instance of type" &
                            " does not denote a type (RM 8.6)", N);
            end if;
            Set_Entity (N, Any_Type);

         else
            --  If the type is an incomplete type created to handle
            --  anonymous access components of a record type, then the
            --  incomplete type is the visible entity and subsequent
            --  references will point to it. Mark the original full
            --  type as referenced, to prevent spurious warnings.

            if Is_Incomplete_Type (T_Name)
              and then Present (Full_View (T_Name))
              and then not Comes_From_Source (T_Name)
            then
               Set_Referenced (Full_View (T_Name));
            end if;

            T_Name := Get_Full_View (T_Name);

            --  Ada 2005 (AI-251, AI-50217): Handle interfaces visible through
            --  limited-with clauses

            if From_Limited_With (T_Name)
              and then Is_Incomplete_Type (T_Name)
              and then Present (Non_Limited_View (T_Name))
              and then Is_Interface (Non_Limited_View (T_Name))
            then
               T_Name := Non_Limited_View (T_Name);
            end if;

            if In_Open_Scopes (T_Name) then
               if Ekind (Base_Type (T_Name)) = E_Task_Type then

                  --  OK if the "current instance" rule does not apply.

                  if not Is_Current_Instance (N) then
                     Set_Entity (N, T_Name);
                     Set_Etype  (N, T_Name);
                     return;

                  else
                     Error_Msg_N
                       ("task type cannot be used as type mark " &
                        "within its own spec or body", N);
                  end if;

               elsif Ekind (Base_Type (T_Name)) = E_Protected_Type then

                  --  OK if the "current instance" rule does not apply.

                  if not Is_Current_Instance (N) then
                     Set_Entity (N, T_Name);
                     Set_Etype  (N, T_Name);
                     return;

                  else
                     Error_Msg_N
                       ("protected type cannot be used as type mark " &
                        "within its own spec or body", N);
                  end if;

               else
                  Error_Msg_N ("type declaration cannot refer to itself", N);
               end if;

               Set_Etype (N, Any_Type);
               Set_Entity (N, Any_Type);
               Set_Error_Posted (T_Name);
               return;
            end if;

            Set_Entity (N, T_Name);
            Set_Etype  (N, T_Name);
         end if;
      end if;

      if Present (Etype (N)) and then Comes_From_Source (N) then
         if Is_Fixed_Point_Type (Etype (N)) then
            Check_Restriction (No_Fixed_Point, N);
         elsif Is_Floating_Point_Type (Etype (N)) then
            Check_Restriction (No_Floating_Point, N);
         end if;

         --  A Ghost type must appear in a specific context

         if Is_Ghost_Entity (Etype (N)) then
            Check_Ghost_Context (Etype (N), N);
         end if;
      end if;
   end Find_Type;

   --------------------
   -- Has_Components --
   --------------------

   function Has_Components (Typ : Entity_Id) return Boolean is
   begin
      return Is_Record_Type (Typ)
        or else (Is_Private_Type (Typ) and then Has_Discriminants (Typ))
        or else (Is_Task_Type (Typ) and then Has_Discriminants (Typ))
        or else (Is_Incomplete_Type (Typ)
                  and then From_Limited_With (Typ)
                  and then Is_Record_Type (Available_View (Typ)));
   end Has_Components;

   ------------------------------------
   -- Has_Implicit_Character_Literal --
   ------------------------------------

   function Has_Implicit_Character_Literal (N : Node_Id) return Boolean is
      Id      : Entity_Id;
      Found   : Boolean := False;
      P       : constant Entity_Id := Entity (Prefix (N));
      Priv_Id : Entity_Id := Empty;

   begin
      if Ekind (P) = E_Package and then not In_Open_Scopes (P) then
         Priv_Id := First_Private_Entity (P);
      end if;

      if P = Standard_Standard then
         Change_Selected_Component_To_Expanded_Name (N);
         Rewrite (N, Selector_Name (N));
         Analyze (N);
         Set_Etype (Original_Node (N), Standard_Character);
         return True;
      end if;

      Id := First_Entity (P);
      while Present (Id) and then Id /= Priv_Id loop
         if Is_Standard_Character_Type (Id) and then Is_Base_Type (Id) then

            --  We replace the node with the literal itself, resolve as a
            --  character, and set the type correctly.

            if not Found then
               Change_Selected_Component_To_Expanded_Name (N);
               Rewrite (N, Selector_Name (N));
               Analyze (N);
               Set_Etype (N, Id);
               Set_Etype (Original_Node (N), Id);
               Found := True;

            else
               --  More than one type derived from Character in given scope.
               --  Collect all possible interpretations.

               Add_One_Interp (N, Id, Id);
            end if;
         end if;

         Next_Entity (Id);
      end loop;

      return Found;
   end Has_Implicit_Character_Literal;

   ----------------------
   -- Has_Private_With --
   ----------------------

   function Has_Private_With (E : Entity_Id) return Boolean is
      Comp_Unit : constant Node_Id := Cunit (Current_Sem_Unit);
      Item      : Node_Id;

   begin
      Item := First (Context_Items (Comp_Unit));
      while Present (Item) loop
         if Nkind (Item) = N_With_Clause
           and then Private_Present (Item)
           and then Is_Entity_Name (Name (Item))
           and then Entity (Name (Item)) = E
         then
            return True;
         end if;

         Next (Item);
      end loop;

      return False;
   end Has_Private_With;

   ---------------------------
   -- Has_Implicit_Operator --
   ---------------------------

   function Has_Implicit_Operator (N : Node_Id) return Boolean is
      Op_Id   : constant Name_Id   := Chars (Selector_Name (N));
      P       : constant Entity_Id := Entity (Prefix (N));
      Id      : Entity_Id;
      Priv_Id : Entity_Id := Empty;

      procedure Add_Implicit_Operator
        (T       : Entity_Id;
         Op_Type : Entity_Id := Empty);
      --  Add implicit interpretation to node N, using the type for which a
      --  predefined operator exists. If the operator yields a boolean type,
      --  the Operand_Type is implicitly referenced by the operator, and a
      --  reference to it must be generated.

      ---------------------------
      -- Add_Implicit_Operator --
      ---------------------------

      procedure Add_Implicit_Operator
        (T       : Entity_Id;
         Op_Type : Entity_Id := Empty)
      is
         Predef_Op : Entity_Id;

      begin
         Predef_Op := Current_Entity (Selector_Name (N));
         while Present (Predef_Op)
           and then Scope (Predef_Op) /= Standard_Standard
         loop
            Predef_Op := Homonym (Predef_Op);
         end loop;

         if Nkind (N) = N_Selected_Component then
            Change_Selected_Component_To_Expanded_Name (N);
         end if;

         --  If the context is an unanalyzed function call, determine whether
         --  a binary or unary interpretation is required.

         if Nkind (Parent (N)) = N_Indexed_Component then
            declare
               Is_Binary_Call : constant Boolean :=
                                  Present
                                    (Next (First (Expressions (Parent (N)))));
               Is_Binary_Op   : constant Boolean :=
                                  First_Entity
                                    (Predef_Op) /= Last_Entity (Predef_Op);
               Predef_Op2     : constant Entity_Id := Homonym (Predef_Op);

            begin
               if Is_Binary_Call then
                  if Is_Binary_Op then
                     Add_One_Interp (N, Predef_Op, T);
                  else
                     Add_One_Interp (N, Predef_Op2, T);
                  end if;
               else
                  if not Is_Binary_Op then
                     Add_One_Interp (N, Predef_Op, T);

                  --  Predef_Op2 may be empty in case of previous errors

                  elsif Present (Predef_Op2) then
                     Add_One_Interp (N, Predef_Op2, T);
                  end if;
               end if;
            end;

         else
            Add_One_Interp (N, Predef_Op, T);

            --  For operators with unary and binary interpretations, if
            --  context is not a call, add both

            if Present (Homonym (Predef_Op)) then
               Add_One_Interp (N, Homonym (Predef_Op), T);
            end if;
         end if;

         --  The node is a reference to a predefined operator, and
         --  an implicit reference to the type of its operands.

         if Present (Op_Type) then
            Generate_Operator_Reference (N, Op_Type);
         else
            Generate_Operator_Reference (N, T);
         end if;
      end Add_Implicit_Operator;

   --  Start of processing for Has_Implicit_Operator

   begin
      if Ekind (P) = E_Package and then not In_Open_Scopes (P) then
         Priv_Id := First_Private_Entity (P);
      end if;

      Id := First_Entity (P);

      case Op_Id is

         --  Boolean operators: an implicit declaration exists if the scope
         --  contains a declaration for a derived Boolean type, or for an
         --  array of Boolean type.

         when Name_Op_And
            | Name_Op_Not
            | Name_Op_Or
            | Name_Op_Xor
         =>
            while Id /= Priv_Id loop
               if Is_Type (Id)
                 and then Valid_Boolean_Arg (Id)
                 and then Is_Base_Type (Id)
               then
                  Add_Implicit_Operator (Id);
                  return True;
               end if;

               Next_Entity (Id);
            end loop;

         --  Equality: look for any non-limited type (result is Boolean)

         when Name_Op_Eq
            | Name_Op_Ne
         =>
            while Id /= Priv_Id loop
               if Is_Type (Id)
                 and then Valid_Equality_Arg (Id)
                 and then Is_Base_Type (Id)
               then
                  Add_Implicit_Operator (Standard_Boolean, Id);
                  return True;
               end if;

               Next_Entity (Id);
            end loop;

         --  Comparison operators: scalar type, or array of scalar

         when Name_Op_Ge
            | Name_Op_Gt
            | Name_Op_Le
            | Name_Op_Lt
         =>
            while Id /= Priv_Id loop
               if Is_Type (Id)
                 and then Valid_Comparison_Arg (Id)
                 and then Is_Base_Type (Id)
               then
                  Add_Implicit_Operator (Standard_Boolean, Id);
                  return True;
               end if;

               Next_Entity (Id);
            end loop;

         --  Arithmetic operators: any numeric type

         when Name_Op_Abs
            | Name_Op_Add
            | Name_Op_Divide
            | Name_Op_Expon
            | Name_Op_Mod
            | Name_Op_Multiply
            | Name_Op_Rem
            | Name_Op_Subtract
         =>
            while Id /= Priv_Id loop
               if Is_Numeric_Type (Id) and then Is_Base_Type (Id) then
                  Add_Implicit_Operator (Id);
                  return True;
               end if;

               Next_Entity (Id);
            end loop;

         --  Concatenation: any one-dimensional array type

         when Name_Op_Concat =>
            while Id /= Priv_Id loop
               if Is_Array_Type (Id)
                 and then Number_Dimensions (Id) = 1
                 and then Is_Base_Type (Id)
               then
                  Add_Implicit_Operator (Id);
                  return True;
               end if;

               Next_Entity (Id);
            end loop;

         --  What is the others condition here? Should we be using a
         --  subtype of Name_Id that would restrict to operators ???

         when others =>
            null;
      end case;

      --  If we fall through, then we do not have an implicit operator

      return False;
   end Has_Implicit_Operator;

   -----------------------------------
   -- Has_Loop_In_Inner_Open_Scopes --
   -----------------------------------

   function Has_Loop_In_Inner_Open_Scopes (S : Entity_Id) return Boolean is
   begin
      --  Several scope stacks are maintained by Scope_Stack. The base of the
      --  currently active scope stack is denoted by the Is_Active_Stack_Base
      --  flag in the scope stack entry. Note that the scope stacks used to
      --  simply be delimited implicitly by the presence of Standard_Standard
      --  at their base, but there now are cases where this is not sufficient
      --  because Standard_Standard actually may appear in the middle of the
      --  active set of scopes.

      for J in reverse 0 .. Scope_Stack.Last loop

         --  S was reached without seing a loop scope first

         if Scope_Stack.Table (J).Entity = S then
            return False;

         --  S was not yet reached, so it contains at least one inner loop

         elsif Ekind (Scope_Stack.Table (J).Entity) = E_Loop then
            return True;
         end if;

         --  Check Is_Active_Stack_Base to tell us when to stop, as there are
         --  cases where Standard_Standard appears in the middle of the active
         --  set of scopes. This affects the declaration and overriding of
         --  private inherited operations in instantiations of generic child
         --  units.

         pragma Assert (not Scope_Stack.Table (J).Is_Active_Stack_Base);
      end loop;

      raise Program_Error;    --  unreachable
   end Has_Loop_In_Inner_Open_Scopes;

   --------------------
   -- In_Open_Scopes --
   --------------------

   function In_Open_Scopes (S : Entity_Id) return Boolean is
   begin
      --  Several scope stacks are maintained by Scope_Stack. The base of the
      --  currently active scope stack is denoted by the Is_Active_Stack_Base
      --  flag in the scope stack entry. Note that the scope stacks used to
      --  simply be delimited implicitly by the presence of Standard_Standard
      --  at their base, but there now are cases where this is not sufficient
      --  because Standard_Standard actually may appear in the middle of the
      --  active set of scopes.

      for J in reverse 0 .. Scope_Stack.Last loop
         if Scope_Stack.Table (J).Entity = S then
            return True;
         end if;

         --  Check Is_Active_Stack_Base to tell us when to stop, as there are
         --  cases where Standard_Standard appears in the middle of the active
         --  set of scopes. This affects the declaration and overriding of
         --  private inherited operations in instantiations of generic child
         --  units.

         exit when Scope_Stack.Table (J).Is_Active_Stack_Base;
      end loop;

      return False;
   end In_Open_Scopes;

   -----------------------------
   -- Inherit_Renamed_Profile --
   -----------------------------

   procedure Inherit_Renamed_Profile (New_S : Entity_Id; Old_S : Entity_Id) is
      New_F : Entity_Id;
      Old_F : Entity_Id;
      Old_T : Entity_Id;
      New_T : Entity_Id;

   begin
      if Ekind (Old_S) = E_Operator then
         New_F := First_Formal (New_S);

         while Present (New_F) loop
            Set_Etype (New_F, Base_Type (Etype (New_F)));
            Next_Formal (New_F);
         end loop;

         Set_Etype (New_S, Base_Type (Etype (New_S)));

      else
         New_F := First_Formal (New_S);
         Old_F := First_Formal (Old_S);

         while Present (New_F) loop
            New_T := Etype (New_F);
            Old_T := Etype (Old_F);

            --  If the new type is a renaming of the old one, as is the case
            --  for actuals in instances, retain its name, to simplify later
            --  disambiguation. Beware of Natural and Positive, see Cstand.

            if Nkind (Parent (New_T)) = N_Subtype_Declaration
              and then Is_Entity_Name (Subtype_Indication (Parent (New_T)))
              and then Entity (Subtype_Indication (Parent (New_T))) = Old_T
              and then Scope (New_T) /= Standard_Standard
            then
               null;
            else
               Set_Etype (New_F, Old_T);
            end if;

            Next_Formal (New_F);
            Next_Formal (Old_F);
         end loop;

         pragma Assert (No (Old_F));

         if Ekind (Old_S) in E_Function | E_Enumeration_Literal then
            Set_Etype (New_S, Etype (Old_S));
         end if;
      end if;
   end Inherit_Renamed_Profile;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Urefs.Init;
   end Initialize;

   -------------------------
   -- Install_Use_Clauses --
   -------------------------

   procedure Install_Use_Clauses
     (Clause             : Node_Id;
      Force_Installation : Boolean := False)
   is
      U : Node_Id;

   begin
      U := Clause;
      while Present (U) loop

         --  Case of USE package

         if Nkind (U) = N_Use_Package_Clause then
            Use_One_Package (U, Name (U), True);

         --  Case of USE TYPE

         else
            Use_One_Type (Subtype_Mark (U), Force => Force_Installation);

         end if;

         Next_Use_Clause (U);
      end loop;
   end Install_Use_Clauses;

   ----------------------
   -- Mark_Use_Clauses --
   ----------------------

   procedure Mark_Use_Clauses (Id : Node_Or_Entity_Id) is
      procedure Mark_Parameters (Call : Entity_Id);
      --  Perform use_type_clause marking for all parameters in a subprogram
      --  or operator call.

      procedure Mark_Use_Package (Pak : Entity_Id);
      --  Move up the Prev_Use_Clause chain for packages denoted by Pak -
      --  marking each clause in the chain as effective in the process.

      procedure Mark_Use_Type (E : Entity_Id);
      --  Similar to Do_Use_Package_Marking except we move up the
      --  Prev_Use_Clause chain for the type denoted by E.

      ---------------------
      -- Mark_Parameters --
      ---------------------

      procedure Mark_Parameters (Call : Entity_Id) is
         Curr : Node_Id;

      begin
         --  Move through all of the formals

         Curr := First_Formal (Call);
         while Present (Curr) loop
            Mark_Use_Type (Curr);

            Next_Formal (Curr);
         end loop;

         --  Handle the return type

         Mark_Use_Type (Call);
      end Mark_Parameters;

      ----------------------
      -- Mark_Use_Package --
      ----------------------

      procedure Mark_Use_Package (Pak : Entity_Id) is
         Curr : Node_Id;

      begin
         --  Ignore cases where the scope of the type is not a package (e.g.
         --  Standard_Standard).

         if Ekind (Pak) /= E_Package then
            return;
         end if;

         Curr := Current_Use_Clause (Pak);
         while Present (Curr)
           and then not Is_Effective_Use_Clause (Curr)
         loop
            --  We need to mark the previous use clauses as effective, but
            --  each use clause may in turn render other use_package_clauses
            --  effective. Additionally, it is possible to have a parent
            --  package renamed as a child of itself so we must check the
            --  prefix entity is not the same as the package we are marking.

            if Nkind (Name (Curr)) /= N_Identifier
              and then Present (Prefix (Name (Curr)))
              and then Entity (Prefix (Name (Curr))) /= Pak
            then
               Mark_Use_Package (Entity (Prefix (Name (Curr))));

            --  It is also possible to have a child package without a prefix
            --  that relies on a previous use_package_clause.

            elsif Nkind (Name (Curr)) = N_Identifier
              and then Is_Child_Unit (Entity (Name (Curr)))
            then
               Mark_Use_Package (Scope (Entity (Name (Curr))));
            end if;

            --  Mark the use_package_clause as effective and move up the chain

            Set_Is_Effective_Use_Clause (Curr);

            Curr := Prev_Use_Clause (Curr);
         end loop;
      end Mark_Use_Package;

      -------------------
      -- Mark_Use_Type --
      -------------------

      procedure Mark_Use_Type (E : Entity_Id) is
         Curr : Node_Id;
         Base : Entity_Id;

      begin
         --  Ignore void types and unresolved string literals and primitives

         if Nkind (E) = N_String_Literal
           or else Nkind (Etype (E)) not in N_Entity
           or else not Is_Type (Etype (E))
         then
            return;
         end if;

         --  Primitives with class-wide operands might additionally render
         --  their base type's use_clauses effective - so do a recursive check
         --  here.

         Base := Base_Type (Etype (E));

         if Ekind (Base) = E_Class_Wide_Type then
            Mark_Use_Type (Base);
         end if;

         --  The package containing the type or operator function being used
         --  may be in use as well, so mark any use_package_clauses for it as
         --  effective. There are also additional sanity checks performed here
         --  for ignoring previous errors.

         Mark_Use_Package (Scope (Base));

         if Nkind (E) in N_Op
           and then Present (Entity (E))
           and then Present (Scope (Entity (E)))
         then
            Mark_Use_Package (Scope (Entity (E)));
         end if;

         Curr := Current_Use_Clause (Base);
         while Present (Curr)
            and then not Is_Effective_Use_Clause (Curr)
         loop
            --  Current use_type_clause may render other use_package_clauses
            --  effective.

            if Nkind (Subtype_Mark (Curr)) /= N_Identifier
              and then Present (Prefix (Subtype_Mark (Curr)))
            then
               Mark_Use_Package (Entity (Prefix (Subtype_Mark (Curr))));
            end if;

            --  Mark the use_type_clause as effective and move up the chain

            Set_Is_Effective_Use_Clause (Curr);

            Curr := Prev_Use_Clause (Curr);
         end loop;
      end Mark_Use_Type;

   --  Start of processing for Mark_Use_Clauses

   begin
      --  Use clauses in and of themselves do not count as a "use" of a
      --  package.

      if Nkind (Parent (Id)) in N_Use_Package_Clause | N_Use_Type_Clause then
         return;
      end if;

      --  Handle entities

      if Nkind (Id) in N_Entity then

         --  Mark the entity's package

         if Is_Potentially_Use_Visible (Id) then
            Mark_Use_Package (Scope (Id));
         end if;

         --  Mark enumeration literals

         if Ekind (Id) = E_Enumeration_Literal then
            Mark_Use_Type (Id);

         --  Mark primitives

         elsif (Is_Overloadable (Id)
                 or else Is_Generic_Subprogram (Id))
           and then (Is_Potentially_Use_Visible (Id)
                      or else Is_Intrinsic_Subprogram (Id)
                      or else (Ekind (Id) in E_Function | E_Procedure
                                and then Is_Generic_Actual_Subprogram (Id)))
         then
            Mark_Parameters (Id);
         end if;

      --  Handle nodes

      else
         --  Mark operators

         if Nkind (Id) in N_Op then

            --  At this point the left operand may not be resolved if we are
            --  encountering multiple operators next to eachother in an
            --  expression.

            if Nkind (Id) in N_Binary_Op
              and then not (Nkind (Left_Opnd (Id)) in N_Op)
            then
               Mark_Use_Type (Left_Opnd (Id));
            end if;

            Mark_Use_Type (Right_Opnd (Id));
            Mark_Use_Type (Id);

         --  Mark entity identifiers

         elsif Nkind (Id) in N_Has_Entity
           and then (Is_Potentially_Use_Visible (Entity (Id))
                      or else (Is_Generic_Instance (Entity (Id))
                                and then Is_Immediately_Visible (Entity (Id))))
         then
            --  Ignore fully qualified names as they do not count as a "use" of
            --  a package.

            if Nkind (Id) in N_Identifier | N_Operator_Symbol
              or else (Present (Prefix (Id))
                         and then Scope (Entity (Id)) /= Entity (Prefix (Id)))
            then
               Mark_Use_Clauses (Entity (Id));
            end if;
         end if;
      end if;
   end Mark_Use_Clauses;

   --------------------------------
   -- Most_Descendant_Use_Clause --
   --------------------------------

   function Most_Descendant_Use_Clause
     (Clause1 : Entity_Id;
      Clause2 : Entity_Id) return Entity_Id
   is
      function Determine_Package_Scope (Clause : Node_Id) return Entity_Id;
      --  Given a use clause, determine which package it belongs to

      -----------------------------
      -- Determine_Package_Scope --
      -----------------------------

      function Determine_Package_Scope (Clause : Node_Id) return Entity_Id is
      begin
         --  Check if the clause appears in the context area

         --  Note we cannot employ Enclosing_Packge for use clauses within
         --  context clauses since they are not actually "enclosed."

         if Nkind (Parent (Clause)) = N_Compilation_Unit then
            return Entity_Of_Unit (Unit (Parent (Clause)));
         end if;

         --  Otherwise, obtain the enclosing package normally

         return Enclosing_Package (Clause);
      end Determine_Package_Scope;

      Scope1 : Entity_Id;
      Scope2 : Entity_Id;

   --  Start of processing for Most_Descendant_Use_Clause

   begin
      if Clause1 = Clause2 then
         return Clause1;
      end if;

      --  We determine which one is the most descendant by the scope distance
      --  to the ultimate parent unit.

      Scope1 := Determine_Package_Scope (Clause1);
      Scope2 := Determine_Package_Scope (Clause2);
      while Scope1 /= Standard_Standard
        and then Scope2 /= Standard_Standard
      loop
         Scope1 := Scope (Scope1);
         Scope2 := Scope (Scope2);

         if No (Scope1) then
            return Clause1;
         elsif No (Scope2) then
            return Clause2;
         end if;
      end loop;

      if Scope1 = Standard_Standard then
         return Clause1;
      end if;

      return Clause2;
   end Most_Descendant_Use_Clause;

   ---------------
   -- Pop_Scope --
   ---------------

   procedure Pop_Scope is
      SST : Scope_Stack_Entry renames Scope_Stack.Table (Scope_Stack.Last);
      S   : constant Scope_Kind_Id := SST.Entity;

   begin
      if Debug_Flag_E then
         Write_Info;
      end if;

      --  Set Default_Storage_Pool field of the library unit if necessary

      if Is_Package_Or_Generic_Package (S)
        and then
          Nkind (Parent (Unit_Declaration_Node (S))) = N_Compilation_Unit
      then
         declare
            Aux : constant Node_Id :=
                    Aux_Decls_Node (Parent (Unit_Declaration_Node (S)));
         begin
            if No (Default_Storage_Pool (Aux)) then
               Set_Default_Storage_Pool (Aux, Default_Pool);
            end if;
         end;
      end if;

      Scope_Suppress           := SST.Save_Scope_Suppress;
      Local_Suppress_Stack_Top := SST.Save_Local_Suppress_Stack_Top;
      Check_Policy_List        := SST.Save_Check_Policy_List;
      Default_Pool             := SST.Save_Default_Storage_Pool;
      No_Tagged_Streams        := SST.Save_No_Tagged_Streams;
      SPARK_Mode               := SST.Save_SPARK_Mode;
      SPARK_Mode_Pragma        := SST.Save_SPARK_Mode_Pragma;
      Default_SSO              := SST.Save_Default_SSO;
      Uneval_Old               := SST.Save_Uneval_Old;

      if Debug_Flag_W then
         Write_Str ("<-- exiting scope: ");
         Write_Name (Chars (Current_Scope));
         Write_Str (", Depth=");
         Write_Int (Int (Scope_Stack.Last));
         Write_Eol;
      end if;

      End_Use_Clauses (SST.First_Use_Clause);

      --  If the actions to be wrapped are still there they will get lost
      --  causing incomplete code to be generated. It is better to abort in
      --  this case (and we do the abort even with assertions off since the
      --  penalty is incorrect code generation).

      if SST.Actions_To_Be_Wrapped /= Scope_Actions'(others => No_List) then
         raise Program_Error;
      end if;

      --  Free last subprogram name if allocated, and pop scope

      Free (SST.Last_Subprogram_Name);
      Scope_Stack.Decrement_Last;
   end Pop_Scope;

   ----------------
   -- Push_Scope --
   ----------------

   procedure Push_Scope (S : Scope_Kind_Id) is
      E : constant Entity_Id := Scope (S);

      function Component_Alignment_Default return Component_Alignment_Kind;
      --  Return Component_Alignment_Kind for the newly-pushed scope.

      function Component_Alignment_Default return Component_Alignment_Kind is
      begin
         --  Each new scope pushed onto the scope stack inherits the component
         --  alignment of the previous scope. This emulates the "visibility"
         --  semantics of pragma Component_Alignment.

         if Scope_Stack.Last > Scope_Stack.First then
            return Scope_Stack.Table
              (Scope_Stack.Last - 1).Component_Alignment_Default;

         --  Otherwise, this is the first scope being pushed on the scope
         --  stack. Inherit the component alignment from the configuration
         --  form of pragma Component_Alignment (if any).

         else
            return Configuration_Component_Alignment;
         end if;
      end Component_Alignment_Default;

   begin
      if Ekind (S) = E_Void then
         null;

      --  Set scope depth if not a nonconcurrent type, and we have not yet set
      --  the scope depth. This means that we have the first occurrence of the
      --  scope, and this is where the depth is set.

      elsif (not Is_Type (S) or else Is_Concurrent_Type (S))
        and then not Scope_Depth_Set (S)
      then
         if S = Standard_Standard then
            Set_Scope_Depth_Value (S, Uint_0);

         elsif Is_Child_Unit (S) then
            Set_Scope_Depth_Value (S, Uint_1);

         elsif not Is_Record_Type (Current_Scope) then
            if Scope_Depth_Set (Current_Scope) then
               if Ekind (S) = E_Loop then
                  Set_Scope_Depth_Value (S, Scope_Depth (Current_Scope));
               else
                  Set_Scope_Depth_Value (S, Scope_Depth (Current_Scope) + 1);
               end if;
            end if;
         end if;
      end if;

      Scope_Stack.Increment_Last;

      Scope_Stack.Table (Scope_Stack.Last) :=
        (Entity                        => S,
         Save_Scope_Suppress           => Scope_Suppress,
         Save_Local_Suppress_Stack_Top => Local_Suppress_Stack_Top,
         Save_Check_Policy_List        => Check_Policy_List,
         Save_Default_Storage_Pool     => Default_Pool,
         Save_No_Tagged_Streams        => No_Tagged_Streams,
         Save_SPARK_Mode               => SPARK_Mode,
         Save_SPARK_Mode_Pragma        => SPARK_Mode_Pragma,
         Save_Default_SSO              => Default_SSO,
         Save_Uneval_Old               => Uneval_Old,
         Component_Alignment_Default   => Component_Alignment_Default,
         Last_Subprogram_Name          => null,
         Is_Transient                  => False,
         Node_To_Be_Wrapped            => Empty,
         Pending_Freeze_Actions        => No_List,
         Actions_To_Be_Wrapped         => (others => No_List),
         First_Use_Clause              => Empty,
         Is_Active_Stack_Base          => False,
         Previous_Visibility           => False,
         Locked_Shared_Objects         => No_Elist);

      if Debug_Flag_W then
         Write_Str ("--> new scope: ");
         Write_Name (Chars (Current_Scope));
         Write_Str (", Id=");
         Write_Int (Int (Current_Scope));
         Write_Str (", Depth=");
         Write_Int (Int (Scope_Stack.Last));
         Write_Eol;
      end if;

      --  Deal with copying flags from the previous scope to this one. This is
      --  not necessary if either scope is standard, or if the new scope is a
      --  child unit.

      if S /= Standard_Standard
        and then Scope (S) /= Standard_Standard
        and then not Is_Child_Unit (S)
      then
         if Nkind (E) not in N_Entity then
            return;
         end if;

         --  Copy categorization flags from Scope (S) to S, this is not done
         --  when Scope (S) is Standard_Standard since propagation is from
         --  library unit entity inwards. Copy other relevant attributes as
         --  well (Discard_Names in particular).

         --  We only propagate inwards for library level entities,
         --  inner level subprograms do not inherit the categorization.

         if Is_Library_Level_Entity (S) then
            Set_Is_Preelaborated  (S, Is_Preelaborated (E));
            Set_Is_Shared_Passive (S, Is_Shared_Passive (E));
            Set_Discard_Names     (S, Discard_Names (E));
            Set_Suppress_Value_Tracking_On_Call
                                  (S, Suppress_Value_Tracking_On_Call (E));
            Set_Categorization_From_Scope (E => S, Scop => E);
         end if;
      end if;

      if Is_Child_Unit (S)
        and then Present (E)
        and then Is_Package_Or_Generic_Package (E)
        and then
          Nkind (Parent (Unit_Declaration_Node (E))) = N_Compilation_Unit
      then
         declare
            Aux : constant Node_Id :=
                    Aux_Decls_Node (Parent (Unit_Declaration_Node (E)));
         begin
            if Present (Default_Storage_Pool (Aux)) then
               Default_Pool := Default_Storage_Pool (Aux);
            end if;
         end;
      end if;
   end Push_Scope;

   ---------------------
   -- Premature_Usage --
   ---------------------

   procedure Premature_Usage (N : Node_Id) is
      Kind : constant Node_Kind := Nkind (Parent (Entity (N)));
      E    : Entity_Id := Entity (N);

   begin
      --  Within an instance, the analysis of the actual for a formal object
      --  does not see the name of the object itself. This is significant only
      --  if the object is an aggregate, where its analysis does not do any
      --  name resolution on component associations. (see 4717-008). In such a
      --  case, look for the visible homonym on the chain.

      if In_Instance and then Present (Homonym (E)) then
         E := Homonym (E);
         while Present (E) and then not In_Open_Scopes (Scope (E)) loop
            E := Homonym (E);
         end loop;

         if Present (E) then
            Set_Entity (N, E);
            Set_Etype (N, Etype (E));
            return;
         end if;
      end if;

      case Kind is
         when N_Component_Declaration =>
            Error_Msg_N
              ("component&! cannot be used before end of record declaration",
               N);

         when N_Parameter_Specification =>
            Error_Msg_N
              ("formal parameter&! cannot be used before end of specification",
               N);

         when N_Discriminant_Specification =>
            Error_Msg_N
              ("discriminant&! cannot be used before end of discriminant part",
               N);

         when N_Procedure_Specification | N_Function_Specification =>
            Error_Msg_N
              ("subprogram&! cannot be used before end of its declaration",
               N);

         when N_Full_Type_Declaration | N_Subtype_Declaration =>
            Error_Msg_N
              ("type& cannot be used before end of its declaration!", N);

         when others =>
            Error_Msg_N
              ("object& cannot be used before end of its declaration!", N);

            --  If the premature reference appears as the expression in its own
            --  declaration, rewrite it to prevent compiler loops in subsequent
            --  uses of this mangled declaration in address clauses.

            if Nkind (Parent (N)) = N_Object_Declaration then
               Set_Entity (N, Any_Id);
            end if;
      end case;
   end Premature_Usage;

   ------------------------
   -- Present_System_Aux --
   ------------------------

   function Present_System_Aux (N : Node_Id := Empty) return Boolean is
      Loc      : Source_Ptr;
      Aux_Name : Unit_Name_Type;
      Unum     : Unit_Number_Type;
      Withn    : Node_Id;
      With_Sys : Node_Id;
      The_Unit : Node_Id;

      function Find_System (C_Unit : Node_Id) return Entity_Id;
      --  Scan context clause of compilation unit to find with_clause
      --  for System.

      -----------------
      -- Find_System --
      -----------------

      function Find_System (C_Unit : Node_Id) return Entity_Id is
         With_Clause : Node_Id;

      begin
         With_Clause := First (Context_Items (C_Unit));
         while Present (With_Clause) loop
            if (Nkind (With_Clause) = N_With_Clause
              and then Chars (Name (With_Clause)) = Name_System)
              and then Comes_From_Source (With_Clause)
            then
               return With_Clause;
            end if;

            Next (With_Clause);
         end loop;

         return Empty;
      end Find_System;

   --  Start of processing for Present_System_Aux

   begin
      --  The child unit may have been loaded and analyzed already

      if Present (System_Aux_Id) then
         return True;

      --  If no previous pragma for System.Aux, nothing to load

      elsif No (System_Extend_Unit) then
         return False;

      --  Use the unit name given in the pragma to retrieve the unit.
      --  Verify that System itself appears in the context clause of the
      --  current compilation. If System is not present, an error will
      --  have been reported already.

      else
         With_Sys := Find_System (Cunit (Current_Sem_Unit));

         The_Unit := Unit (Cunit (Current_Sem_Unit));

         if No (With_Sys)
           and then
             (Nkind (The_Unit) = N_Package_Body
               or else (Nkind (The_Unit) = N_Subprogram_Body
                         and then not Acts_As_Spec (Cunit (Current_Sem_Unit))))
         then
            With_Sys :=
              Find_System (Spec_Or_Body_Lib_Unit (Cunit (Current_Sem_Unit)));
         end if;

         if No (With_Sys) and then Present (N) then

            --  If we are compiling a subunit, we need to examine its
            --  context as well (Current_Sem_Unit is the parent unit);

            The_Unit := Parent (N);
            while Nkind (The_Unit) /= N_Compilation_Unit loop
               The_Unit := Parent (The_Unit);
            end loop;

            if Nkind (Unit (The_Unit)) = N_Subunit then
               With_Sys := Find_System (The_Unit);
            end if;
         end if;

         if No (With_Sys) then
            return False;
         end if;

         Loc := Sloc (With_Sys);
         Get_Name_String (Chars (Expression (System_Extend_Unit)));
         Name_Buffer (8 .. Name_Len + 7) := Name_Buffer (1 .. Name_Len);
         Name_Buffer (1 .. 7) := "system.";
         Name_Buffer (Name_Len + 8) := '%';
         Name_Buffer (Name_Len + 9) := 's';
         Name_Len := Name_Len + 9;
         Aux_Name := Name_Find;

         Unum :=
           Load_Unit
             (Load_Name  => Aux_Name,
              Required   => False,
              Subunit    => False,
              Error_Node => With_Sys);

         if Unum /= No_Unit then
            Semantics (Cunit (Unum));
            System_Aux_Id :=
              Defining_Entity (Specification (Unit (Cunit (Unum))));

            Withn :=
              Make_With_Clause (Loc,
                Name =>
                  Make_Expanded_Name (Loc,
                    Chars         => Chars (System_Aux_Id),
                    Prefix        =>
                      New_Occurrence_Of (Scope (System_Aux_Id), Loc),
                    Selector_Name => New_Occurrence_Of (System_Aux_Id, Loc)));

            Set_Entity (Name (Withn), System_Aux_Id);

            Set_Corresponding_Spec (Withn, System_Aux_Id);
            Set_First_Name         (Withn);
            Set_Is_Implicit_With   (Withn);
            Set_Withed_Lib_Unit    (Withn, Cunit (Unum));

            Insert_After (With_Sys, Withn);
            Mark_Rewrite_Insertion (Withn);
            Set_Context_Installed (Withn);

            return True;

         --  Here if unit load failed

         else
            Error_Msg_Name_1 := Name_System;
            Error_Msg_Name_2 := Chars (Expression (System_Extend_Unit));
            Error_Msg_N
              ("extension package `%.%` does not exist",
               Opt.System_Extend_Unit);
            return False;
         end if;
      end if;
   end Present_System_Aux;

   -------------------------
   -- Restore_Scope_Stack --
   -------------------------

   procedure Restore_Scope_Stack
     (List       : Elist_Id;
      Handle_Use : Boolean := True)
   is
      SS_Last : constant Int := Scope_Stack.Last;
      Elmt    : Elmt_Id;

   begin
      --  Restore visibility of previous scope stack, if any, using the list
      --  we saved (we use Remove, since this list will not be used again).

      loop
         Elmt := First_Elmt (List);
         exit when Elmt = No_Elmt;
         Set_Is_Immediately_Visible (Node (Elmt));
         Remove_Elmt (List, Elmt);
      end loop;

      --  Restore use clauses

      if SS_Last >= Scope_Stack.First
        and then Scope_Stack.Table (SS_Last).Entity /= Standard_Standard
        and then Handle_Use
      then
         Install_Use_Clauses
           (Scope_Stack.Table (SS_Last).First_Use_Clause,
            Force_Installation => True);
      end if;
   end Restore_Scope_Stack;

   ----------------------
   -- Save_Scope_Stack --
   ----------------------

   --  Save_Scope_Stack/Restore_Scope_Stack were originally designed to avoid
   --  consuming any memory. That is, Save_Scope_Stack took care of removing
   --  from immediate visibility entities and Restore_Scope_Stack took care
   --  of restoring their visibility analyzing the context of each entity. The
   --  problem of such approach is that it was fragile and caused unexpected
   --  visibility problems, and indeed one test was found where there was a
   --  real problem.

   --  Furthermore, the following experiment was carried out:

   --    - Save_Scope_Stack was modified to store in an Elist1 all those
   --      entities whose attribute Is_Immediately_Visible is modified
   --      from True to False.

   --    - Restore_Scope_Stack was modified to store in another Elist2
   --      all the entities whose attribute Is_Immediately_Visible is
   --      modified from False to True.

   --    - Extra code was added to verify that all the elements of Elist1
   --      are found in Elist2

   --  This test shows that there may be more occurrences of this problem which
   --  have not yet been detected. As a result, we replaced that approach by
   --  the current one in which Save_Scope_Stack returns the list of entities
   --  whose visibility is changed, and that list is passed to Restore_Scope_
   --  Stack to undo that change. This approach is simpler and safer, although
   --  it consumes more memory.

   function Save_Scope_Stack (Handle_Use : Boolean := True) return Elist_Id is
      Result  : constant Elist_Id := New_Elmt_List;
      E       : Entity_Id;
      S       : Entity_Id;
      SS_Last : constant Int := Scope_Stack.Last;

      procedure Remove_From_Visibility (E : Entity_Id);
      --  If E is immediately visible then append it to the result and remove
      --  it temporarily from visibility.

      ----------------------------
      -- Remove_From_Visibility --
      ----------------------------

      procedure Remove_From_Visibility (E : Entity_Id) is
      begin
         if Is_Immediately_Visible (E) then
            Append_Elmt (E, Result);
            Set_Is_Immediately_Visible (E, False);
         end if;
      end Remove_From_Visibility;

   --  Start of processing for Save_Scope_Stack

   begin
      if SS_Last >= Scope_Stack.First
        and then Scope_Stack.Table (SS_Last).Entity /= Standard_Standard
      then
         if Handle_Use then
            End_Use_Clauses (Scope_Stack.Table (SS_Last).First_Use_Clause);
         end if;

         --  If the call is from within a compilation unit, as when called from
         --  Rtsfind, make current entries in scope stack invisible while we
         --  analyze the new unit.

         for J in reverse 0 .. SS_Last loop
            exit when  Scope_Stack.Table (J).Entity = Standard_Standard
               or else No (Scope_Stack.Table (J).Entity);

            S := Scope_Stack.Table (J).Entity;

            Remove_From_Visibility (S);

            E := First_Entity (S);
            while Present (E) loop
               Remove_From_Visibility (E);
               Next_Entity (E);
            end loop;
         end loop;

      end if;

      return Result;
   end Save_Scope_Stack;

   -------------
   -- Set_Use --
   -------------

   procedure Set_Use (L : List_Id) is
      Decl : Node_Id;

   begin
      Decl := First (L);
      while Present (Decl) loop
         if Nkind (Decl) = N_Use_Package_Clause then
            Chain_Use_Clause (Decl);
            Use_One_Package (Decl, Name (Decl));

         elsif Nkind (Decl) = N_Use_Type_Clause then
            Chain_Use_Clause (Decl);
            Use_One_Type (Subtype_Mark (Decl));

         end if;

         Next (Decl);
      end loop;
   end Set_Use;

   -----------------------------
   -- Update_Use_Clause_Chain --
   -----------------------------

   procedure Update_Use_Clause_Chain is

      procedure Update_Chain_In_Scope (Level : Int);
      --  Iterate through one level in the scope stack verifying each use-type
      --  clause within said level is used then reset the Current_Use_Clause
      --  to a redundant use clause outside of the current ending scope if such
      --  a clause exists.

      ---------------------------
      -- Update_Chain_In_Scope --
      ---------------------------

      procedure Update_Chain_In_Scope (Level : Int) is
         Curr : Node_Id;
         N    : Node_Id;

      begin
         --  Loop through all use clauses within the scope dictated by Level

         Curr := Scope_Stack.Table (Level).First_Use_Clause;
         while Present (Curr) loop

            --  Retrieve the subtype mark or name within the current current
            --  use clause.

            if Nkind (Curr) = N_Use_Type_Clause then
               N := Subtype_Mark (Curr);
            else
               N := Name (Curr);
            end if;

            --  If warnings for unreferenced entities are enabled and the
            --  current use clause has not been marked effective.

            if Check_Unreferenced
              and then Comes_From_Source (Curr)
              and then not Is_Effective_Use_Clause (Curr)
              and then not In_Instance
              and then not In_Inlined_Body
            then
               --  We are dealing with a potentially unused use_package_clause

               if Nkind (Curr) = N_Use_Package_Clause then

                  --  Renamings and formal subprograms may cause the associated
                  --  node to be marked as effective instead of the original.

                  if not (Present (Associated_Node (N))
                           and then Present
                                      (Current_Use_Clause
                                        (Associated_Node (N)))
                           and then Is_Effective_Use_Clause
                                      (Current_Use_Clause
                                        (Associated_Node (N))))
                  then
                     Error_Msg_Node_1 := Entity (N);
                     Error_Msg_NE
                       ("use clause for package & has no effect?u?",
                        Curr, Entity (N));
                  end if;

               --  We are dealing with an unused use_type_clause

               else
                  Error_Msg_Node_1 := Etype (N);
                  Error_Msg_NE
                    ("use clause for } has no effect?u?", Curr, Etype (N));
               end if;
            end if;

            --  Verify that we haven't already processed a redundant
            --  use_type_clause within the same scope before we move the
            --  current use clause up to a previous one for type T.

            if Present (Prev_Use_Clause (Curr)) then
               Set_Current_Use_Clause (Entity (N), Prev_Use_Clause (Curr));
            end if;

            Next_Use_Clause (Curr);
         end loop;
      end Update_Chain_In_Scope;

   --  Start of processing for Update_Use_Clause_Chain

   begin
      Update_Chain_In_Scope (Scope_Stack.Last);

      --  Deal with use clauses within the context area if the current
      --  scope is a compilation unit.

      if Is_Compilation_Unit (Current_Scope)
        and then Sloc (Scope_Stack.Table
                        (Scope_Stack.Last - 1).Entity) = Standard_Location
      then
         Update_Chain_In_Scope (Scope_Stack.Last - 1);
      end if;
   end Update_Use_Clause_Chain;

   ---------------------
   -- Use_One_Package --
   ---------------------

   procedure Use_One_Package
     (N         : Node_Id;
      Pack_Name : Entity_Id := Empty;
      Force     : Boolean   := False)
   is
      procedure Note_Redundant_Use (Clause : Node_Id);
      --  Mark the name in a use clause as redundant if the corresponding
      --  entity is already use-visible. Emit a warning if the use clause comes
      --  from source and the proper warnings are enabled.

      ------------------------
      -- Note_Redundant_Use --
      ------------------------

      procedure Note_Redundant_Use (Clause : Node_Id) is
         Decl      : constant Node_Id   := Parent (Clause);
         Pack_Name : constant Entity_Id := Entity (Clause);

         Cur_Use    : Node_Id := Current_Use_Clause (Pack_Name);
         Prev_Use   : Node_Id := Empty;
         Redundant  : Node_Id := Empty;
         --  The Use_Clause which is actually redundant. In the simplest case
         --  it is Pack itself, but when we compile a body we install its
         --  context before that of its spec, in which case it is the
         --  use_clause in the spec that will appear to be redundant, and we
         --  want the warning to be placed on the body. Similar complications
         --  appear when the redundancy is between a child unit and one of its
         --  ancestors.

      begin
         --  Could be renamed...

         if No (Cur_Use) then
            Cur_Use := Current_Use_Clause (Renamed_Entity (Pack_Name));
         end if;

         Set_Redundant_Use (Clause, True);

         --  Do not check for redundant use if clause is generated, or in an
         --  instance, or in a predefined unit to avoid misleading warnings
         --  that may occur as part of a rtsfind load.

         if not Comes_From_Source (Clause)
           or else In_Instance
           or else not Warn_On_Redundant_Constructs
           or else Is_Predefined_Unit (Current_Sem_Unit)
         then
            return;
         end if;

         if not Is_Compilation_Unit (Current_Scope) then

            --  If the use_clause is in an inner scope, it is made redundant by
            --  some clause in the current context, with one exception: If we
            --  are compiling a nested package body, and the use_clause comes
            --  from then corresponding spec, the clause is not necessarily
            --  fully redundant, so we should not warn. If a warning was
            --  warranted, it would have been given when the spec was
            --  processed.

            if Nkind (Parent (Decl)) = N_Package_Specification then
               declare
                  Package_Spec_Entity : constant Entity_Id :=
                                          Defining_Unit_Name (Parent (Decl));
               begin
                  if In_Package_Body (Package_Spec_Entity) then
                     return;
                  end if;
               end;
            end if;

            Redundant := Clause;
            Prev_Use  := Cur_Use;

         elsif Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Body then
            declare
               Cur_Unit : constant Unit_Number_Type :=
                            Get_Source_Unit (Cur_Use);
               New_Unit : constant Unit_Number_Type :=
                            Get_Source_Unit (Clause);

               Scop : Entity_Id;

            begin
               if Cur_Unit = New_Unit then

                  --  Redundant clause in same body

                  Redundant := Clause;
                  Prev_Use  := Cur_Use;

               elsif Cur_Unit = Current_Sem_Unit then

                  --  If the new clause is not in the current unit it has been
                  --  analyzed first, and it makes the other one redundant.
                  --  However, if the new clause appears in a subunit, Cur_Unit
                  --  is still the parent, and in that case the redundant one
                  --  is the one appearing in the subunit.

                  if Nkind (Unit (Cunit (New_Unit))) = N_Subunit then
                     Redundant := Clause;
                     Prev_Use  := Cur_Use;

                  --  Most common case: redundant clause in body, original
                  --  clause in spec. Current scope is spec entity.

                  elsif Current_Scope = Cunit_Entity (Current_Sem_Unit) then
                     Redundant := Cur_Use;
                     Prev_Use  := Clause;

                  else
                     --  The new clause may appear in an unrelated unit, when
                     --  the parents of a generic are being installed prior to
                     --  instantiation. In this case there must be no warning.
                     --  We detect this case by checking whether the current
                     --  top of the stack is related to the current
                     --  compilation.

                     Scop := Current_Scope;
                     while Present (Scop)
                       and then Scop /= Standard_Standard
                     loop
                        if Is_Compilation_Unit (Scop)
                          and then not Is_Child_Unit (Scop)
                        then
                           return;

                        elsif Scop = Cunit_Entity (Current_Sem_Unit) then
                           exit;
                        end if;

                        Scop := Scope (Scop);
                     end loop;

                     Redundant := Cur_Use;
                     Prev_Use  := Clause;
                  end if;

               elsif New_Unit = Current_Sem_Unit then
                  Redundant := Clause;
                  Prev_Use  := Cur_Use;

               else
                  --  Neither is the current unit, so they appear in parent or
                  --  sibling units. Warning will be emitted elsewhere.

                  return;
               end if;
            end;

         elsif Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Declaration
           and then Present (Parent_Spec (Unit (Cunit (Current_Sem_Unit))))
         then
            --  Use_clause is in child unit of current unit, and the child unit
            --  appears in the context of the body of the parent, so it has
            --  been installed first, even though it is the redundant one.
            --  Depending on their placement in the context, the visible or the
            --  private parts of the two units, either might appear as
            --  redundant, but the message has to be on the current unit.

            if Get_Source_Unit (Cur_Use) = Current_Sem_Unit then
               Redundant := Cur_Use;
               Prev_Use  := Clause;
            else
               Redundant := Clause;
               Prev_Use  := Cur_Use;
            end if;

            --  If the new use clause appears in the private part of a parent
            --  unit it may appear to be redundant w.r.t. a use clause in a
            --  child unit, but the previous use clause was needed in the
            --  visible part of the child, and no warning should be emitted.

            if Nkind (Parent (Decl)) = N_Package_Specification
              and then List_Containing (Decl) =
                         Private_Declarations (Parent (Decl))
            then
               declare
                  Par      : constant Entity_Id :=
                    Defining_Entity (Parent (Decl));
                  Spec     : constant Node_Id  :=
                    Specification (Unit (Cunit (Current_Sem_Unit)));
                  Cur_List : constant List_Id := List_Containing (Cur_Use);

               begin
                  if Is_Compilation_Unit (Par)
                    and then Par /= Cunit_Entity (Current_Sem_Unit)
                  then
                     if Cur_List = Context_Items (Cunit (Current_Sem_Unit))
                       or else Cur_List = Visible_Declarations (Spec)
                     then
                        return;
                     end if;
                  end if;
               end;
            end if;

         --  Finally, if the current use clause is in the context then the
         --  clause is redundant when it is nested within the unit.

         elsif Nkind (Parent (Cur_Use)) = N_Compilation_Unit
           and then Nkind (Parent (Parent (Clause))) /= N_Compilation_Unit
           and then Get_Source_Unit (Cur_Use) = Get_Source_Unit (Clause)
         then
            Redundant := Clause;
            Prev_Use  := Cur_Use;
         end if;

         if Present (Redundant) and then Parent (Redundant) /= Prev_Use then

            --  Make sure we are looking at most-descendant use_package_clause
            --  by traversing the chain with Find_First_Use and then verifying
            --  there is no scope manipulation via Most_Descendant_Use_Clause.

            if Nkind (Prev_Use) = N_Use_Package_Clause
              and then
                (Nkind (Parent (Prev_Use)) /= N_Compilation_Unit
                  or else Most_Descendant_Use_Clause
                            (Prev_Use, Find_First_Use (Prev_Use)) /= Prev_Use)
            then
               Prev_Use := Find_First_Use (Prev_Use);
            end if;

            Error_Msg_Sloc := Sloc (Prev_Use);
            Error_Msg_NE -- CODEFIX
              ("& is already use-visible through previous use_clause #?r?",
               Redundant, Pack_Name);
         end if;
      end Note_Redundant_Use;

      --  Local variables

      Current_Instance : Entity_Id := Empty;
      Id               : Entity_Id;
      P                : Entity_Id;
      Prev             : Entity_Id;
      Private_With_OK  : Boolean   := False;
      Real_P           : Entity_Id;

   --  Start of processing for Use_One_Package

   begin
      --  Use_One_Package may have been called recursively to handle an
      --  implicit use for a auxiliary system package, so set P accordingly
      --  and skip redundancy checks.

      if No (Pack_Name) and then Present_System_Aux (N) then
         P := System_Aux_Id;

      --  Check for redundant use_package_clauses

      else
         --  Ignore cases where we are dealing with a non user defined package
         --  like Standard_Standard or something other than a valid package.

         if not Is_Entity_Name (Pack_Name)
           or else No (Entity (Pack_Name))
           or else Ekind (Entity (Pack_Name)) /= E_Package
         then
            return;
         end if;

         --  When a renaming exists we must check it for redundancy. The
         --  original package would have already been seen at this point.

         if Present (Renamed_Entity (Entity (Pack_Name))) then
            P := Renamed_Entity (Entity (Pack_Name));
         else
            P := Entity (Pack_Name);
         end if;

         --  Check for redundant clauses then set the current use clause for
         --  P if were are not "forcing" an installation from a scope
         --  reinstallation that is done throughout analysis for various
         --  reasons.

         if In_Use (P) then
            Note_Redundant_Use (Pack_Name);

            if not Force then
               Set_Current_Use_Clause (P, N);
            end if;

            return;

         --  Warn about detected redundant clauses

         elsif not Force
           and then In_Open_Scopes (P)
           and then not Is_Hidden_Open_Scope (P)
         then
            if Warn_On_Redundant_Constructs and then P = Current_Scope then
               Error_Msg_NE -- CODEFIX
                 ("& is already use-visible within itself?r?",
                   Pack_Name, P);
            end if;

            return;
         end if;

         --  Set P back to the non-renamed package so that visibility of the
         --  entities within the package can be properly set below.

         P := Entity (Pack_Name);
      end if;

      Set_In_Use (P);
      Set_Current_Use_Clause (P, N);

      --  Ada 2005 (AI-50217): Check restriction

      if From_Limited_With (P) then
         Error_Msg_N ("limited withed package cannot appear in use clause", N);
      end if;

      --  Find enclosing instance, if any

      if In_Instance then
         Current_Instance := Current_Scope;
         while not Is_Generic_Instance (Current_Instance) loop
            Current_Instance := Scope (Current_Instance);
         end loop;

         if No (Hidden_By_Use_Clause (N)) then
            Set_Hidden_By_Use_Clause (N, New_Elmt_List);
         end if;
      end if;

      --  If unit is a package renaming, indicate that the renamed package is
      --  also in use (the flags on both entities must remain consistent, and a
      --  subsequent use of either of them should be recognized as redundant).

      if Present (Renamed_Entity (P)) then
         Set_In_Use (Renamed_Entity (P));
         Set_Current_Use_Clause (Renamed_Entity (P), N);
         Real_P := Renamed_Entity (P);
      else
         Real_P := P;
      end if;

      --  Ada 2005 (AI-262): Check the use_clause of a private withed package
      --  found in the private part of a package specification

      if In_Private_Part (Current_Scope)
        and then Has_Private_With (P)
        and then Is_Child_Unit (Current_Scope)
        and then Is_Child_Unit (P)
        and then Is_Ancestor_Package (Scope (Current_Scope), P)
      then
         Private_With_OK := True;
      end if;

      --  Loop through entities in one package making them potentially
      --  use-visible.

      Id := First_Entity (P);
      while Present (Id)
        and then (Id /= First_Private_Entity (P)
                   or else Private_With_OK) -- Ada 2005 (AI-262)
      loop
         Prev := Current_Entity (Id);
         while Present (Prev) loop
            if Is_Immediately_Visible (Prev)
              and then (not Is_Overloadable (Prev)
                         or else not Is_Overloadable (Id)
                         or else Type_Conformant (Id, Prev))
            then
               if No (Current_Instance) then

                  --  Potentially use-visible entity remains hidden

                  if Warn_On_Hiding then
                     Warn_On_Hiding_Entity (N, Hidden => Id, Visible => Prev,
                                            On_Use_Clause => True);
                  end if;

                  goto Next_Usable_Entity;

               --  A use clause within an instance hides outer global entities,
               --  which are not used to resolve local entities in the
               --  instance. Note that the predefined entities in Standard
               --  could not have been hidden in the generic by a use clause,
               --  and therefore remain visible. Other compilation units whose
               --  entities appear in Standard must be hidden in an instance.

               --  To determine whether an entity is external to the instance
               --  we compare the scope depth of its scope with that of the
               --  current instance. However, a generic actual of a subprogram
               --  instance is declared in the wrapper package but will not be
               --  hidden by a use-visible entity. similarly, an entity that is
               --  declared in an enclosing instance will not be hidden by an
               --  an entity declared in a generic actual, which can only have
               --  been use-visible in the generic and will not have hidden the
               --  entity in the generic parent.

               --  If Id is called Standard, the predefined package with the
               --  same name is in the homonym chain. It has to be ignored
               --  because it has no defined scope (being the only entity in
               --  the system with this mandated behavior).

               elsif not Is_Hidden (Id)
                 and then Present (Scope (Prev))
                 and then not Is_Wrapper_Package (Scope (Prev))
                 and then Scope_Depth (Scope (Prev)) <
                          Scope_Depth (Current_Instance)
                 and then (Scope (Prev) /= Standard_Standard
                            or else Sloc (Prev) > Standard_Location)
               then
                  if In_Open_Scopes (Scope (Prev))
                    and then Is_Generic_Instance (Scope (Prev))
                    and then Present (Associated_Formal_Package (P))
                  then
                     null;

                  else
                     Set_Is_Potentially_Use_Visible (Id);
                     Set_Is_Immediately_Visible (Prev, False);
                     Append_Elmt (Prev, Hidden_By_Use_Clause (N));
                  end if;
               end if;

            --  A user-defined operator is not use-visible if the predefined
            --  operator for the type is immediately visible, which is the case
            --  if the type of the operand is in an open scope. This does not
            --  apply to user-defined operators that have operands of different
            --  types, because the predefined mixed mode operations (multiply
            --  and divide) apply to universal types and do not hide anything.

            elsif Ekind (Prev) = E_Operator
              and then Operator_Matches_Spec (Prev, Id)
              and then In_Open_Scopes
                         (Scope (Base_Type (Etype (First_Formal (Id)))))
              and then (No (Next_Formal (First_Formal (Id)))
                         or else Etype (First_Formal (Id)) =
                                 Etype (Next_Formal (First_Formal (Id)))
                         or else Chars (Prev) = Name_Op_Expon)
            then
               goto Next_Usable_Entity;

            --  In an instance, two homonyms may become use_visible through the
            --  actuals of distinct formal packages. In the generic, only the
            --  current one would have been visible, so make the other one
            --  not use_visible.

            --  In certain pathological cases it is possible that unrelated
            --  homonyms from distinct formal packages may exist in an
            --  uninstalled scope. We must test for that here.

            elsif Present (Current_Instance)
              and then Is_Potentially_Use_Visible (Prev)
              and then not Is_Overloadable (Prev)
              and then Scope (Id) /= Scope (Prev)
              and then Used_As_Generic_Actual (Scope (Prev))
              and then Used_As_Generic_Actual (Scope (Id))
              and then Is_List_Member (Scope (Prev))
              and then not In_Same_List (Current_Use_Clause (Scope (Prev)),
                                         Current_Use_Clause (Scope (Id)))
            then
               Set_Is_Potentially_Use_Visible (Prev, False);
               Append_Elmt (Prev, Hidden_By_Use_Clause (N));
            end if;

            Prev := Homonym (Prev);
         end loop;

         --  On exit, we know entity is not hidden, unless it is private

         if not Is_Hidden (Id)
           and then (not Is_Child_Unit (Id) or else Is_Visible_Lib_Unit (Id))
         then
            Set_Is_Potentially_Use_Visible (Id);

            if Is_Private_Type (Id) and then Present (Full_View (Id)) then
               Set_Is_Potentially_Use_Visible (Full_View (Id));
            end if;
         end if;

         <<Next_Usable_Entity>>
            Next_Entity (Id);
      end loop;

      --  Child units are also made use-visible by a use clause, but they may
      --  appear after all visible declarations in the parent entity list.

      while Present (Id) loop
         if Is_Child_Unit (Id) and then Is_Visible_Lib_Unit (Id) then
            Set_Is_Potentially_Use_Visible (Id);
         end if;

         Next_Entity (Id);
      end loop;

      if Chars (Real_P) = Name_System
        and then Scope (Real_P) = Standard_Standard
        and then Present_System_Aux (N)
      then
         Use_One_Package (N);
      end if;
   end Use_One_Package;

   ------------------
   -- Use_One_Type --
   ------------------

   procedure Use_One_Type
     (Id        : Node_Id;
      Installed : Boolean := False;
      Force     : Boolean := False)
   is
      function Spec_Reloaded_For_Body return Boolean;
      --  Determine whether the compilation unit is a package body and the use
      --  type clause is in the spec of the same package. Even though the spec
      --  was analyzed first, its context is reloaded when analysing the body.

      procedure Use_Class_Wide_Operations (Typ : Entity_Id);
      --  AI05-150: if the use_type_clause carries the "all" qualifier,
      --  class-wide operations of ancestor types are use-visible if the
      --  ancestor type is visible.

      ----------------------------
      -- Spec_Reloaded_For_Body --
      ----------------------------

      function Spec_Reloaded_For_Body return Boolean is
      begin
         if Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Body then
            declare
               Spec : constant Node_Id :=
                        Parent (List_Containing (Parent (Id)));

            begin
               --  Check whether type is declared in a package specification,
               --  and current unit is the corresponding package body. The
               --  use clauses themselves may be within a nested package.

               return
                 Nkind (Spec) = N_Package_Specification
                   and then In_Same_Source_Unit
                              (Corresponding_Body (Parent (Spec)),
                               Cunit_Entity (Current_Sem_Unit));
            end;
         end if;

         return False;
      end Spec_Reloaded_For_Body;

      -------------------------------
      -- Use_Class_Wide_Operations --
      -------------------------------

      procedure Use_Class_Wide_Operations (Typ : Entity_Id) is
         function Is_Class_Wide_Operation_Of
           (Op : Entity_Id;
            T  : Entity_Id) return Boolean;
         --  Determine whether a subprogram has a class-wide parameter or
         --  result that is T'Class.

         ---------------------------------
         --  Is_Class_Wide_Operation_Of --
         ---------------------------------

         function Is_Class_Wide_Operation_Of
           (Op : Entity_Id;
            T  : Entity_Id) return Boolean
         is
            Formal : Entity_Id;

         begin
            Formal := First_Formal (Op);
            while Present (Formal) loop
               if Etype (Formal) = Class_Wide_Type (T) then
                  return True;
               end if;

               Next_Formal (Formal);
            end loop;

            if Etype (Op) = Class_Wide_Type (T) then
               return True;
            end if;

            return False;
         end Is_Class_Wide_Operation_Of;

         --  Local variables

         Ent  : Entity_Id;
         Scop : Entity_Id;

      --  Start of processing for Use_Class_Wide_Operations

      begin
         Scop := Scope (Typ);
         if not Is_Hidden (Scop) then
            Ent := First_Entity (Scop);
            while Present (Ent) loop
               if Is_Overloadable (Ent)
                 and then Is_Class_Wide_Operation_Of (Ent, Typ)
                 and then not Is_Potentially_Use_Visible (Ent)
               then
                  Set_Is_Potentially_Use_Visible (Ent);
                  Append_Elmt (Ent, Used_Operations (Parent (Id)));
               end if;

               Next_Entity (Ent);
            end loop;
         end if;

         if Is_Derived_Type (Typ) then
            Use_Class_Wide_Operations (Etype (Base_Type (Typ)));
         end if;
      end Use_Class_Wide_Operations;

      --  Local variables

      Elmt          : Elmt_Id;
      Is_Known_Used : Boolean;
      Op_List       : Elist_Id;
      T             : Entity_Id;

   --  Start of processing for Use_One_Type

   begin
      if Entity (Id) = Any_Type then
         return;
      end if;

      --  It is the type determined by the subtype mark (8.4(8)) whose
      --  operations become potentially use-visible.

      T := Base_Type (Entity (Id));

      --  Either the type itself is used, the package where it is declared is
      --  in use or the entity is declared in the current package, thus
      --  use-visible.

      Is_Known_Used :=
          (In_Use (T)
            and then ((Present (Current_Use_Clause (T))
                        and then All_Present (Current_Use_Clause (T)))
                      or else not All_Present (Parent (Id))))
        or else In_Use (Scope (T))
        or else Scope (T) = Current_Scope;

      Set_Redundant_Use (Id,
        Is_Known_Used or else Is_Potentially_Use_Visible (T));

      if Ekind (T) = E_Incomplete_Type then
         Error_Msg_N ("premature usage of incomplete type", Id);

      elsif In_Open_Scopes (Scope (T)) then
         null;

      --  A limited view cannot appear in a use_type_clause. However, an access
      --  type whose designated type is limited has the flag but is not itself
      --  a limited view unless we only have a limited view of its enclosing
      --  package.

      elsif From_Limited_With (T) and then From_Limited_With (Scope (T)) then
         Error_Msg_N
           ("incomplete type from limited view cannot appear in use clause",
            Id);

      --  If the use clause is redundant, Used_Operations will usually be
      --  empty, but we need to set it to empty here in one case: If we are
      --  instantiating a generic library unit, then we install the ancestors
      --  of that unit in the scope stack, which involves reprocessing use
      --  clauses in those ancestors. Such a use clause will typically have a
      --  nonempty Used_Operations unless it was redundant in the generic unit,
      --  even if it is redundant at the place of the instantiation.

      elsif Redundant_Use (Id) then
         Set_Used_Operations (Parent (Id), New_Elmt_List);

      --  If the subtype mark designates a subtype in a different package,
      --  we have to check that the parent type is visible, otherwise the
      --  use_type_clause is a no-op. Not clear how to do that???

      else
         Set_Current_Use_Clause (T, Parent (Id));
         Set_In_Use (T);

         --  If T is tagged, primitive operators on class-wide operands are
         --  also deemed available. Note that this is really necessary only
         --  in semantics-only mode, because the primitive operators are not
         --  fully constructed in this mode, but we do it in all modes for the
         --  sake of uniformity, as this should not matter in practice.

         if Is_Tagged_Type (T) then
            Set_In_Use (Class_Wide_Type (T));
         end if;

         --  Iterate over primitive operations of the type. If an operation is
         --  already use_visible, it is the result of a previous use_clause,
         --  and already appears on the corresponding entity chain. If the
         --  clause is being reinstalled, operations are already use-visible.

         if Installed then
            null;

         else
            Op_List := Collect_Primitive_Operations (T);
            Elmt := First_Elmt (Op_List);
            while Present (Elmt) loop
               if (Nkind (Node (Elmt)) = N_Defining_Operator_Symbol
                    or else Chars (Node (Elmt)) in Any_Operator_Name)
                 and then not Is_Hidden (Node (Elmt))
                 and then not Is_Potentially_Use_Visible (Node (Elmt))
               then
                  Set_Is_Potentially_Use_Visible (Node (Elmt));
                  Append_Elmt (Node (Elmt), Used_Operations (Parent (Id)));

               elsif Ada_Version >= Ada_2012
                 and then All_Present (Parent (Id))
                 and then not Is_Hidden (Node (Elmt))
                 and then not Is_Potentially_Use_Visible (Node (Elmt))
               then
                  Set_Is_Potentially_Use_Visible (Node (Elmt));
                  Append_Elmt (Node (Elmt), Used_Operations (Parent (Id)));
               end if;

               Next_Elmt (Elmt);
            end loop;
         end if;

         if Ada_Version >= Ada_2012
           and then All_Present (Parent (Id))
           and then Is_Tagged_Type (T)
         then
            Use_Class_Wide_Operations (T);
         end if;
      end if;

      --  If warning on redundant constructs, check for unnecessary WITH

      if not Force
        and then Warn_On_Redundant_Constructs
        and then Is_Known_Used

        --                     with P;         with P; use P;
        --    package P is     package X is    package body X is
        --       type T ...       use P.T;

        --  The compilation unit is the body of X. GNAT first compiles the
        --  spec of X, then proceeds to the body. At that point P is marked
        --  as use visible. The analysis then reinstalls the spec along with
        --  its context. The use clause P.T is now recognized as redundant,
        --  but in the wrong context. Do not emit a warning in such cases.
        --  Do not emit a warning either if we are in an instance, there is
        --  no redundancy between an outer use_clause and one that appears
        --  within the generic.

        and then not Spec_Reloaded_For_Body
        and then not In_Instance
        and then not In_Inlined_Body
      then
         --  The type already has a use clause

         if In_Use (T) then

            --  Case where we know the current use clause for the type

            if Present (Current_Use_Clause (T)) then
               Use_Clause_Known : declare
                  Clause1 : constant Node_Id :=
                              Find_First_Use (Current_Use_Clause (T));
                  Clause2 : constant Node_Id := Parent (Id);
                  Ent1    : Entity_Id;
                  Ent2    : Entity_Id;
                  Err_No  : Node_Id;
                  Unit1   : Node_Id;
                  Unit2   : Node_Id;

               --  Start of processing for Use_Clause_Known

               begin
                  --  If the unit is a subprogram body that acts as spec, the
                  --  context clause is shared with the constructed subprogram
                  --  spec. Clearly there is no redundancy.

                  if Clause1 = Clause2 then
                     return;
                  end if;

                  Unit1 := Unit (Enclosing_Comp_Unit_Node (Clause1));
                  Unit2 := Unit (Enclosing_Comp_Unit_Node (Clause2));

                  --  If both clauses are on same unit, or one is the body of
                  --  the other, or one of them is in a subunit, report
                  --  redundancy on the later one.

                  if Unit1 = Unit2
                    or else Nkind (Unit1) = N_Subunit
                    or else
                      (Nkind (Unit2) in N_Package_Body | N_Subprogram_Body
                        and then Nkind (Unit1) /= Nkind (Unit2)
                        and then Nkind (Unit1) /= N_Subunit)
                  then
                     Error_Msg_Sloc := Sloc (Clause1);
                     Error_Msg_NE -- CODEFIX
                       ("& is already use-visible through previous "
                        & "use_type_clause #?r?", Clause2, T);
                     return;
                  end if;

                  --  If there is a redundant use_type_clause in a child unit
                  --  determine which of the units is more deeply nested. If a
                  --  unit is a package instance, retrieve the entity and its
                  --  scope from the instance spec.

                  Ent1 := Entity_Of_Unit (Unit1);
                  Ent2 := Entity_Of_Unit (Unit2);

                  --  When the scope of both units' entities are
                  --  Standard_Standard then neither Unit1 or Unit2 are child
                  --  units - so return in that case.

                  if Scope (Ent1) = Standard_Standard
                    and then Scope (Ent2) = Standard_Standard
                  then
                     return;

                  --  Otherwise, determine if one of the units is not a child

                  elsif Scope (Ent2) = Standard_Standard then
                     Error_Msg_Sloc := Sloc (Clause2);
                     Err_No := Clause1;

                  elsif Scope (Ent1) = Standard_Standard then
                     Error_Msg_Sloc := Sloc (Id);
                     Err_No := Clause2;

                  --  If both units are child units, we determine which one is
                  --  the descendant by the scope distance to the ultimate
                  --  parent unit.

                  else
                     declare
                        S1 : Entity_Id;
                        S2 : Entity_Id;

                     begin
                        S1 := Scope (Ent1);
                        S2 := Scope (Ent2);
                        while Present (S1)
                          and then Present (S2)
                          and then S1 /= Standard_Standard
                          and then S2 /= Standard_Standard
                        loop
                           S1 := Scope (S1);
                           S2 := Scope (S2);
                        end loop;

                        if S1 = Standard_Standard then
                           Error_Msg_Sloc := Sloc (Id);
                           Err_No := Clause2;
                        else
                           Error_Msg_Sloc := Sloc (Clause2);
                           Err_No := Clause1;
                        end if;
                     end;
                  end if;

                  if Parent (Id) /= Err_No then
                     if Most_Descendant_Use_Clause
                          (Err_No, Parent (Id)) = Parent (Id)
                     then
                        Error_Msg_Sloc := Sloc (Err_No);
                        Err_No := Parent (Id);
                     end if;

                     Error_Msg_NE -- CODEFIX
                       ("& is already use-visible through previous "
                        & "use_type_clause #?r?", Err_No, Id);
                  end if;
               end Use_Clause_Known;

            --  Here Current_Use_Clause is not set for T, so we do not have the
            --  location information available.

            else
               Error_Msg_NE -- CODEFIX
                 ("& is already use-visible through previous "
                  & "use_type_clause?r?", Id, T);
            end if;

         --  The package where T is declared is already used

         elsif In_Use (Scope (T)) then
            --  Due to expansion of contracts we could be attempting to issue
            --  a spurious warning - so verify there is a previous use clause.

            if Current_Use_Clause (Scope (T)) /=
                 Find_First_Use (Current_Use_Clause (Scope (T)))
            then
               Error_Msg_Sloc :=
                 Sloc (Find_First_Use (Current_Use_Clause (Scope (T))));
               Error_Msg_NE -- CODEFIX
                 ("& is already use-visible through package use clause #?r?",
                  Id, T);
            end if;

         --  The current scope is the package where T is declared

         else
            Error_Msg_Node_2 := Scope (T);
            Error_Msg_NE -- CODEFIX
              ("& is already use-visible inside package &?r?", Id, T);
         end if;
      end if;
   end Use_One_Type;

   ----------------
   -- Write_Info --
   ----------------

   procedure Write_Info is
      Id : Entity_Id := First_Entity (Current_Scope);

   begin
      --  No point in dumping standard entities

      if Current_Scope = Standard_Standard then
         return;
      end if;

      Write_Str ("========================================================");
      Write_Eol;
      Write_Str ("        Defined Entities in ");
      Write_Name (Chars (Current_Scope));
      Write_Eol;
      Write_Str ("========================================================");
      Write_Eol;

      if No (Id) then
         Write_Str ("-- none --");
         Write_Eol;

      else
         while Present (Id) loop
            Write_Entity_Info (Id, " ");
            Next_Entity (Id);
         end loop;
      end if;

      if Scope (Current_Scope) = Standard_Standard then

         --  Print information on the current unit itself

         Write_Entity_Info (Current_Scope, " ");
      end if;

      Write_Eol;
   end Write_Info;

   --------
   -- ws --
   --------

   procedure ws is
      S : Entity_Id;
   begin
      for J in reverse 1 .. Scope_Stack.Last loop
         S := Scope_Stack.Table (J).Entity;
         Write_Int (Int (S));
         Write_Str (" === ");
         Write_Name (Chars (S));
         Write_Eol;
      end loop;
   end ws;

   --------
   -- we --
   --------

   procedure we (S : Entity_Id) is
      E : Entity_Id;
   begin
      E := First_Entity (S);
      while Present (E) loop
         Write_Int (Int (E));
         Write_Str (" === ");
         Write_Name (Chars (E));
         Write_Eol;
         Next_Entity (E);
      end loop;
   end we;
end Sem_Ch8;
