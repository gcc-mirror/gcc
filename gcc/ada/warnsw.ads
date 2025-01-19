------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               W A R N S W                                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1999-2025, Free Software Foundation, Inc.         --
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

--  This unit contains the routines used to handle setting of warning options
--  in the GNAT compiler.

package Warnsw is

   -------------------
   -- Warning Flags --
   -------------------

   --  These flags are activated or deactivated by -gnatw switches and control
   --  whether warnings of a given class will be generated or not.

   package Warnings_Package is
      type Opt_Warnings_Enum is
        --  List of all warnings that can be enabled, plus the two null-ish
        --  values.
        (No_Such_Warning, Special_Case, -- see body

         Address_Clause_Overlay_Warnings,
         Check_Unreferenced,
         Check_Unreferenced_Formals,
         Check_Withs,
         Constant_Condition_Warnings,
         Elab_Info_Messages,
         Elab_Warnings,
         Implementation_Unit_Warnings,
         Ineffective_Inline_Warnings,
         List_Body_Required_Info,
         List_Inherited_Aspects,
         Warning_Doc_Switch,
         Warn_On_Ada_2005_Compatibility,
         Warn_On_Ada_2012_Compatibility,
         Warn_On_Ada_2022_Compatibility,
         Warn_On_All_Unread_Out_Parameters,
         Warn_On_Anonymous_Allocators,
         Warn_On_Assertion_Failure,
         Warn_On_Assumed_Low_Bound,
         Warn_On_Atomic_Synchronization,
         Warn_On_Bad_Fixed_Value,
         Warn_On_Biased_Representation,
         Warn_On_Component_Order,
         Warn_On_Constant,
         Warn_On_Deleted_Code,
         Warn_On_Dereference,
         Warn_On_Elab_Access,
         Warn_On_Export_Import,
         Warn_On_GNAT_Extension_Compatibility,
         Warn_On_Hiding,
         Warn_On_Ignored_Equality,
         Warn_On_Ineffective_Predicate_Test,
         Warn_On_Inherently_Limited_Type,
         Warn_On_Late_Primitives,
         Warn_On_Modified_Unread,
         Warn_On_No_Value_Assigned,
         Warn_On_Non_Dispatching_Primitives,
         Warn_On_Non_Local_Exception,
         No_Warn_On_Non_Local_Exception,
         Warn_On_Object_Renames_Function,
         Warn_On_Obsolescent_Feature,
         Warn_On_Overlap,
         Warn_On_Overridden_Size,
         Warn_On_Parameter_Order,
         Warn_On_Pedantic_Checks,
         Warn_On_Questionable_Layout,
         Warn_On_Questionable_Missing_Parens,
         Warn_On_Record_Holes,
         Warn_On_Redundant_Constructs,
         Warn_On_Reverse_Bit_Order,
         Warn_On_Size_Alignment,
         Warn_On_Standard_Redefinition,
         Warn_On_Suspicious_Contract,
         Warn_On_Suspicious_Modulus_Value,
         Warn_On_Unchecked_Conversion,
         Warn_On_Unknown_Compile_Time_Warning,
         Warn_On_Unordered_Enumeration_Type,
         Warn_On_Unrecognized_Pragma,
         Warn_On_Unrepped_Components,
         Warn_On_Warnings_Off); -- Opt_Warnings_Enum

      subtype Warnings_Enum is Opt_Warnings_Enum
        range Opt_Warnings_Enum'Succ (Special_Case) .. Opt_Warnings_Enum'Last;
      --  Just the warning switches, without the null-ish values
   end Warnings_Package;
   use Warnings_Package;

   type Warnings_State is array (Warnings_Enum) of Boolean;
   pragma Pack (Warnings_State);
   --  Without Pack, we can have bootstrapping failures, because the compiler
   --  generates calls to System.Boolean_Array_Operations, which is not
   --  currently part of the compiler.

   Warning_Flags : Warnings_State :=
   --  Current state of warnings -- True/False means enabled/disabled.
   --  The following initializes the flags to their default values,
   --  and warning switches modify them.
     (Address_Clause_Overlay_Warnings |
      Elab_Warnings |
      Implementation_Unit_Warnings |
      Warning_Doc_Switch |
      Warn_On_Ada_2005_Compatibility |
      Warn_On_Ada_2012_Compatibility |
      Warn_On_Ada_2022_Compatibility |
      Warn_On_Assertion_Failure |
      Warn_On_Assumed_Low_Bound |
      Warn_On_Biased_Representation |
      Warn_On_Export_Import |
      Warn_On_GNAT_Extension_Compatibility |
      Warn_On_No_Value_Assigned |
      Warn_On_Questionable_Missing_Parens |
      Warn_On_Reverse_Bit_Order |
      Warn_On_Size_Alignment |
      Warn_On_Suspicious_Contract |
      Warn_On_Suspicious_Modulus_Value |
      Warn_On_Unchecked_Conversion |
      Warn_On_Unknown_Compile_Time_Warning |
      Warn_On_Unrecognized_Pragma =>
        True,

      Check_Unreferenced |
      Check_Unreferenced_Formals |
      Check_Withs |
      Constant_Condition_Warnings |
      Elab_Info_Messages |
      Ineffective_Inline_Warnings |
      List_Body_Required_Info |
      List_Inherited_Aspects |
      Warn_On_All_Unread_Out_Parameters |
      Warn_On_Anonymous_Allocators |
      Warn_On_Atomic_Synchronization |
      Warn_On_Bad_Fixed_Value |
      Warn_On_Component_Order |
      Warn_On_Constant |
      Warn_On_Deleted_Code |
      Warn_On_Dereference |
      Warn_On_Elab_Access |
      Warn_On_Hiding |
      Warn_On_Ignored_Equality |
      Warn_On_Ineffective_Predicate_Test |
      Warn_On_Inherently_Limited_Type |
      Warn_On_Late_Primitives |
      Warn_On_Modified_Unread |
      Warn_On_Non_Dispatching_Primitives |
      Warn_On_Non_Local_Exception |
      No_Warn_On_Non_Local_Exception |
      Warn_On_Object_Renames_Function |
      Warn_On_Obsolescent_Feature |
      Warn_On_Overlap |
      Warn_On_Overridden_Size |
      Warn_On_Parameter_Order |
      Warn_On_Pedantic_Checks |
      Warn_On_Questionable_Layout |
      Warn_On_Record_Holes |
      Warn_On_Redundant_Constructs |
      Warn_On_Standard_Redefinition |
      Warn_On_Unordered_Enumeration_Type |
      Warn_On_Unrepped_Components |
      Warn_On_Warnings_Off =>
        False);

   package X renames Warnings_Package;
   F : Warnings_State renames Warning_Flags;
   --  Short-hand names used only locally to this package

   --  The following rename all the components of Warning_Flags for convenient
   --  access throughout the compiler.

   pragma Style_Checks ("M120");
   Address_Clause_Overlay_Warnings : Boolean renames F (X.Address_Clause_Overlay_Warnings);
   --  Set False to disable address clause warnings. Modified by use of
   --  -gnatwo/O.

   Check_Unreferenced : Boolean renames F (X.Check_Unreferenced);
   --  Set to True to enable checking for unreferenced entities other
   --  than formal parameters (for which see Check_Unreferenced_Formals)
   --  Modified by use of -gnatwu/U.

   Check_Unreferenced_Formals : Boolean renames F (X.Check_Unreferenced_Formals);
   --  Set to True to check for unreferenced formals. This is turned on by
   --  -gnatwa/wf/wu and turned off by -gnatwA/wF/wU.

   Check_Withs : Boolean renames F (X.Check_Withs);
   --  Set to True to enable checking for unused withs, and also the case
   --  of withing a package and using none of the entities in the package.
   --  Modified by use of -gnatwu/U.

   Constant_Condition_Warnings : Boolean renames F (X.Constant_Condition_Warnings);
   --  Set to True to activate warnings on constant conditions. Modified by
   --  use of -gnatwc/C.

   Elab_Info_Messages : Boolean renames F (X.Elab_Info_Messages);
   --  Set to True to output info messages for static elabmodel (-gnatel)

   Elab_Warnings : Boolean renames F (X.Elab_Warnings);
   --  Set to True to generate elaboration warnings (-gnatwl). The warnings are
   --  enabled by default because they carry the same importance as errors. The
   --  compiler cannot emit actual errors because elaboration diagnostics need
   --  dataflow analysis, which is not available. This behavior parallels that
   --  of the old ABE mechanism.

   Implementation_Unit_Warnings : Boolean renames F (X.Implementation_Unit_Warnings);
   --  Set True to activate warnings for use of implementation internal units.
   --  Modified by use of -gnatwi/-gnatwI.

   Ineffective_Inline_Warnings : Boolean renames F (X.Ineffective_Inline_Warnings);
   --  Set True to activate warnings if front-end inlining (-gnatN) is not able
   --  to actually inline a particular call (or all calls). Can be controlled
   --  by use of -gnatwp/-gnatwP. Also set True to activate warnings if
   --  frontend inlining is not able to inline a subprogram expected to
   --  be inlined in GNATprove mode.

   List_Body_Required_Info : Boolean renames F (X.List_Body_Required_Info);
   --  List info messages about why a package requires a body. Modified by use
   --  of -gnatw.y/.Y.

   List_Inherited_Aspects : Boolean renames F (X.List_Inherited_Aspects);
   --  List inherited invariants, preconditions, and postconditions from
   --  Invariant'Class, Pre'Class, and Post'Class aspects. Also list inherited
   --  subtype predicates. Modified by use of -gnatw.l/.L.

   Warning_Doc_Switch : Boolean renames F (X.Warning_Doc_Switch);
   --  If this is set True, then the ??/?*?/?$?/?x?/?.x?/?_x? insertion
   --  sequences in error messages generate appropriate tags for the output
   --  error messages. If this switch is False, then these sequences are still
   --  recognized (for the purposes of implementing the pattern matching in
   --  pragmas Warnings (Off,..) and Warning_As_Error(...) but do not result
   --  in adding the error message tag. The -gnatw.d switch sets this flag
   --  True, -gnatw.D sets this flag False.

   Warn_On_Ada_2005_Compatibility : Boolean renames F (X.Warn_On_Ada_2005_Compatibility);
   --  Set to True to generate all warnings on Ada 2005 compatibility issues,
   --  including warnings on Ada 2005 obsolescent features used in Ada 2005
   --  mode. Set by default, modified by use of -gnatwy/Y.

   Warn_On_Ada_2012_Compatibility : Boolean renames F (X.Warn_On_Ada_2012_Compatibility);
   --  Set to True to generate all warnings on Ada 2012 compatibility issues,
   --  including warnings on Ada 2012 obsolescent features used in Ada 2012
   --  mode. Modified by use of -gnatwy/Y.

   Warn_On_Ada_2022_Compatibility : Boolean renames F (X.Warn_On_Ada_2022_Compatibility);
   --  Set to True to generate all warnings on Ada 2022 compatibility issues,
   --  including warnings on Ada 2022 obsolescent features used in Ada 2022
   --  mode. There is no switch controlling this option.

   Warn_On_All_Unread_Out_Parameters : Boolean renames F (X.Warn_On_All_Unread_Out_Parameters);
   --  Set to True to generate warnings in all cases where a variable is
   --  modified by being passed as to an OUT formal, but the resulting value is
   --  never read. The default is that this warning is suppressed. Modified
   --  by use of gnatw.o/.O.

   Warn_On_Anonymous_Allocators : Boolean renames F (X.Warn_On_Anonymous_Allocators);
   --  Warn when allocators for anonymous access types are present, which,
   --  although not illegal in Ada, may be confusing to users due to how
   --  accessibility checks get generated. Off by default, modified by use
   --  of -gnatw_a/_A and set as part of -gnatwa.

   Warn_On_Assertion_Failure : Boolean renames F (X.Warn_On_Assertion_Failure);
   --  Set to True to activate warnings on assertions that can be determined
   --  at compile time will always fail. Modified by use of -gnatw.a/.A.

   Warn_On_Assumed_Low_Bound : Boolean renames F (X.Warn_On_Assumed_Low_Bound);
   --  Set to True to activate warnings for string parameters that are indexed
   --  with literals or S'Length, presumably assuming a lower bound of one.
   --  Modified by use of -gnatww/W.

   Warn_On_Atomic_Synchronization : Boolean renames F (X.Warn_On_Atomic_Synchronization);
   --  Set to True to generate information messages for atomic synchronization.
   --  Modified by use of -gnatw.n/.N.

   Warn_On_Bad_Fixed_Value : Boolean renames F (X.Warn_On_Bad_Fixed_Value);
   --  Set to True to generate warnings for static fixed-point expression
   --  values that are not an exact multiple of the small value of the type.
   --  Odd by default, modified by use of -gnatwb/B.

   Warn_On_Biased_Representation : Boolean renames F (X.Warn_On_Biased_Representation);
   --  Set to True to generate warnings for size clauses, component clauses
   --  and component_size clauses that force biased representation. Modified
   --  by use of -gnatw.b/.B.

   Warn_On_Component_Order : Boolean renames F (X.Warn_On_Component_Order);
   --  Warn when record component clauses are out of order with respect to the
   --  component declarations, or if the memory layout is out of order with
   --  respect to component declarations and clauses.  Off by default, set by
   --  -gnatw_r (but not -gnatwa).

   Warn_On_Constant : Boolean renames F (X.Warn_On_Constant);
   --  Set to True to generate warnings for variables that could be declared
   --  as constants. Modified by use of -gnatwk/K.

   Warn_On_Deleted_Code : Boolean renames F (X.Warn_On_Deleted_Code);
   --  Set to True to generate warnings for code deleted by the front end
   --  for conditional statements whose outcome is known at compile time.
   --  Modified by use of -gnatwt/T.

   Warn_On_Dereference : Boolean renames F (X.Warn_On_Dereference);
   --  Set to True to generate warnings for implicit dereferences for array
   --  indexing and record component access. Modified by use of -gnatwd/D.

   Warn_On_Elab_Access : Boolean renames F (X.Warn_On_Elab_Access);
   --  Set to True to generate warnings for P'Access in the case where
   --  subprogram P is in the same package as the P'Access, and the P'Access is
   --  evaluated at package elaboration time, and occurs before the body of P
   --  has been elaborated. Modified by use of -gnatw.f/.F.

   Warn_On_Export_Import : Boolean renames F (X.Warn_On_Export_Import);
   --  Set to True to generate warnings for suspicious use of export or
   --  import pragmas. Modified by use of -gnatwx/X.

   Warn_On_GNAT_Extension_Compatibility : Boolean renames F (X.Warn_On_GNAT_Extension_Compatibility);
   --  Set to True to generate all warnings on GNAT extension compatibility
   --  issues. There is no switch controlling this option.

   Warn_On_Hiding : Boolean renames F (X.Warn_On_Hiding);
   --  Set to True to generate warnings if a declared entity hides another
   --  entity. The default is that this warning is suppressed. Modified by
   --  use of -gnatwh/H.

   Warn_On_Ignored_Equality : Boolean renames F (X.Warn_On_Ignored_Equality);
   --  Warn when a user-defined "=" function does not compose (i.e. is ignored
   --  for a predefined "=" for a composite type containing a component of
   --  whose type has the user-defined "=" as primitive). Off by default, and
   --  set by -gnatw_q (but not -gnatwa).

   Warn_On_Ineffective_Predicate_Test : Boolean renames F (X.Warn_On_Ineffective_Predicate_Test);
   --  Set to True to generate warnings if a static predicate is testing for
   --  values that do not belong to the parent subtype. Modified by use of
   --  -gnatw_s/S.

   Warn_On_Inherently_Limited_Type : Boolean renames F (X.Warn_On_Inherently_Limited_Type);
   --  Set to True to generate warnings if a record type does not have a
   --  limited keyword, but is inherently limited. Modified by use of
   --  -gnatw_l/L.

   Warn_On_Late_Primitives : Boolean renames F (X.Warn_On_Late_Primitives);
   --  Warn when tagged type public primitives are defined after its private
   --  extensions.

   Warn_On_Modified_Unread : Boolean renames F (X.Warn_On_Modified_Unread);
   --  Set to True to generate warnings if a variable is assigned but is never
   --  read. Also controls warnings for similar cases involving out parameters,
   --  but only if there is only one out parameter for the procedure involved.
   --  The default is that this warning is suppressed, modified by use of
   --  -gnatwm/M.

   Warn_On_No_Value_Assigned : Boolean renames F (X.Warn_On_No_Value_Assigned);
   --  Set to True to generate warnings if no value is ever assigned to a
   --  variable that is at least partially uninitialized. Set to false to
   --  suppress such warnings. The default is that such warnings are enabled.
   --  Modified by use of -gnatwv/V.

   Warn_On_Non_Dispatching_Primitives : Boolean renames F (X.Warn_On_Non_Dispatching_Primitives);
   --  Set to True to generate warnings for non dispatching primitives of tagged
   --  types that have aspect/pragma First_Controlling_Parameter set to True.
   --  This is turned on by -gnatw_j and turned off by -gnatw_J

   Warn_On_Non_Local_Exception : Boolean renames F (X.Warn_On_Non_Local_Exception);
   --  Set to True to generate warnings for non-local exception raises and also
   --  handlers that can never handle a local raise. This warning is only ever
   --  generated if pragma Restrictions (No_Exception_Propagation) is set. The
   --  default is not to generate the warnings except that if the source has
   --  at least one exception handler, and this restriction is set, and the
   --  warning was not explicitly turned off, then it is turned on by default.
   --  Modified by use of -gnatw.x/.X.

   No_Warn_On_Non_Local_Exception : Boolean renames F (X.No_Warn_On_Non_Local_Exception);
   --  This is set to True if the above warning is explicitly suppressed. We
   --  use this to avoid turning it on by default when No_Exception_Propagation
   --  restriction is set and an exception handler is present.

   Warn_On_Object_Renames_Function : Boolean renames F (X.Warn_On_Object_Renames_Function);
   --  Set to True to generate warnings when a function result is renamed as
   --  an object. The default is that this warning is disabled. Modified by
   --  use of -gnatw.r/.R.

   Warn_On_Obsolescent_Feature : Boolean renames F (X.Warn_On_Obsolescent_Feature);
   --  Set to True to generate warnings on use of any feature in Annex or if a
   --  subprogram is called for which a pragma Obsolescent applies. Modified
   --  by use of -gnatwj/J.

   Warn_On_Overlap : Boolean renames F (X.Warn_On_Overlap);
   --  Set to True to generate warnings when a writable actual overlaps with
   --  another actual in a subprogram call. This applies only in modes before
   --  Ada 2012. Starting with Ada 2012, such overlaps are illegal.
   --  Modified by use of -gnatw.i/.I.

   Warn_On_Overridden_Size : Boolean renames F (X.Warn_On_Overridden_Size);
   --  Warn when explicit record component clause or array component_size
   --  clause specifies a size that overrides a size for the type which was
   --  set with an explicit size clause. Off by default, modified by use of
   --  -gnatw.s/.S (but not -gnatwa).

   Warn_On_Parameter_Order : Boolean renames F (X.Warn_On_Parameter_Order);
   --  Set to True to generate warnings for cases where the argument list for
   --  a call is a sequence of identifiers that match the formal identifiers,
   --  but are in the wrong order.

   Warn_On_Pedantic_Checks : Boolean renames F (X.Warn_On_Pedantic_Checks);
   --  Warn for violation of miscellaneous pedantic rules (such as when the
   --  subtype of a formal parameter given in a subprogram body's specification
   --  comes from a different subtype declaration that the subtype of the
   --  formal in the subprogram declaration). Off by default, and set by
   --  -gnatw_p (but not -gnatwa).

   Warn_On_Questionable_Layout : Boolean renames F (X.Warn_On_Questionable_Layout);
   --  Warn when default layout of a record type is questionable for run-time
   --  efficiency reasons and would be improved by reordering the components.
   --  Off by default, modified by use of -gnatw.q/.Q (but not -gnatwa).

   function Get_Warn_On_Questionable_Layout return Boolean is
     (Warn_On_Questionable_Layout);
   --  WARNING: There is a matching C declaration of this function in fe.h

   Warn_On_Questionable_Missing_Parens : Boolean renames F (X.Warn_On_Questionable_Missing_Parens);
   --  Set to True to generate warnings for cases where parentheses are missing
   --  and the usage is questionable, because the intent is unclear. On by
   --  default, modified by use of -gnatwq/Q.

   Warn_On_Record_Holes : Boolean renames F (X.Warn_On_Record_Holes);
   --  Warn when explicit record component clauses leave uncovered holes (gaps)
   --  in a record layout. Off by default, set by -gnatw.h (but not -gnatwa).

   Warn_On_Redundant_Constructs : Boolean renames F (X.Warn_On_Redundant_Constructs);
   --  Set to True to generate warnings for redundant constructs (e.g. useless
   --  assignments/conversions). The default is that this warning is disabled.
   --  Modified by use of -gnatwr/R.

   Warn_On_Reverse_Bit_Order : Boolean renames F (X.Warn_On_Reverse_Bit_Order);
   --  Set to True to generate warning (informational) messages for component
   --  clauses that are affected by non-standard bit-order. The default is
   --  that this warning is enabled. Modified by -gnatw.v/.V.

   Warn_On_Size_Alignment : Boolean renames F (X.Warn_On_Size_Alignment);
   --  Warn when explicit Size and Alignment clauses are given for a type, and
   --  the size is not a multiple of the alignment. Off by default, modified
   --  by use of -gnatw.z/.Z and set as part of -gnatwa.

   Warn_On_Standard_Redefinition : Boolean renames F (X.Warn_On_Standard_Redefinition);
   --  Warn when a program defines an identifier that matches a name in
   --  Standard. Off by default, modified by use of -gnatw.k/.K (but not
   --  by -gnatwa).

   Warn_On_Suspicious_Contract : Boolean renames F (X.Warn_On_Suspicious_Contract);
   --  Set to True to generate warnings for suspicious contracts expressed as
   --  pragmas or aspects precondition and postcondition, as well as other
   --  suspicious cases of expressions typically found in contracts like
   --  quantified expressions and uses of Update attribute. The default is that
   --  this warning is enabled. Modified by use of -gnatw.t/.T.

   Warn_On_Suspicious_Modulus_Value : Boolean renames F (X.Warn_On_Suspicious_Modulus_Value);
   --  Set to True to generate warnings for suspicious modulus values, as well
   --  as negative literals of a modular type. The default is that this warning
   --  is enabled. Modified by -gnatw.m/.M.

   Warn_On_Unchecked_Conversion : Boolean renames F (X.Warn_On_Unchecked_Conversion);
   --  Set to True to generate warnings for unchecked conversions that may have
   --  non-portable semantics (e.g. because sizes of types differ). Modified
   --  by use of -gnatwz/Z.

   Warn_On_Unknown_Compile_Time_Warning : Boolean renames F (X.Warn_On_Unknown_Compile_Time_Warning);
   --  Warn on a pragma Compile_Time_Warning whose condition has a value that
   --  is not known at compile time. On by default, modified by use
   --  of -gnatw_c/_C and set as part of -gnatwa.

   Warn_On_Unordered_Enumeration_Type : Boolean renames F (X.Warn_On_Unordered_Enumeration_Type);
   --  Set to True to generate warnings for inappropriate uses (comparisons
   --  and explicit ranges) on unordered enumeration types (which includes
   --  all enumeration types for which pragma Ordered is not given). The
   --  default is that this warning is disabled. Modified by -gnat.u/.U.

   Warn_On_Unrecognized_Pragma : Boolean renames F (X.Warn_On_Unrecognized_Pragma);
   --  Set to True to generate warnings for unrecognized pragmas. The default
   --  is that this warning is enabled. Modified by use of -gnatwg/G.

   Warn_On_Unrepped_Components : Boolean renames F (X.Warn_On_Unrepped_Components);
   --  Set to True to generate warnings for the case of components of record
   --  which have a record representation clause but this component does not
   --  have a component clause. Modified by use of -gnatw.c/.C.

   Warn_On_Warnings_Off : Boolean renames F (X.Warn_On_Warnings_Off);
   --  Set to True to generate warnings for use of Pragma Warnings (Off, ent),
   --  where either the pragma is never used, or it could be replaced by a
   --  pragma Unmodified or Unreferenced. Also generates warnings for pragma
   --  Warning (Off, string) which either has no matching pragma Warning On,
   --  or where no warning has been suppressed by the use of the pragma.
   --  Modified by use of -gnatw.w/.W.
   pragma Style_Checks ("M79");

   -----------------------------------
   -- Saving and Restoring Warnings --
   -----------------------------------

   function Save_Warnings return Warnings_State;
   --  Returns current settings of warnings

   procedure Restore_Warnings (W : Warnings_State);
   --  Restores current settings of warning flags from W

   -----------------
   -- Subprograms --
   -----------------

   type Warning_Family is
     --  The "family" indicates the form of warning switch:
     (Plain, -- form "-gnatwx"
      '.',   -- form "-gnatw.x"
      '_');  -- form "-gnatw_x"
     --  where "x" is a lowercase letter that enables a particular warning.
     --  Typically, uppercase of "x" disables the warning, but there are some
     --  switches that do not follow that pattern.

   function Set_Warning_Switch
     (Family : Warning_Family; C : Character) return Boolean;
   --  Set the warning switch or switches corresponding to the given family and
   --  character. Returns True for valid warning switch, False for invalid.
   --  Called for -gnatw... switches, and for pragma Warnings.

   procedure Set_GNAT_Mode_Warnings;
   --  Called for -gnatg and -gnatw.g to set GNAT mode warnings. This set of
   --  warnings is neither a subset nor a superset of -gnatwa.

end Warnsw;
