------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T 1 D R V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Back_End; use Back_End;
with Comperr;
with Csets;    use Csets;
with Debug;    use Debug;
with Elists;
with Errout;   use Errout;
with Exp_CG;
with Exp_Ch6;  use Exp_Ch6;
with Fmap;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Frontend;
with Gnatvsn;  use Gnatvsn;
with Hostparm;
with Inline;
with Lib;      use Lib;
with Lib.Writ; use Lib.Writ;
with Lib.Xref;
with Namet;    use Namet;
with Nlists;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Par_SCO;
with Prepcomp;
with Repinfo;  use Repinfo;
with Restrict;
with Rident;   use Rident;
with Rtsfind;
with SCOs;
with Sem;
with Sem_Ch8;
with Sem_Ch12;
with Sem_Ch13;
with Sem_Elim;
with Sem_Eval;
with Sem_Type;
with Set_Targ;
with Sinfo;    use Sinfo;
with Sinput.L; use Sinput.L;
with Snames;
with Sprint;   use Sprint;
with Stringt;
with Stylesw;  use Stylesw;
with Targparm; use Targparm;
with Tree_Gen;
with Treepr;   use Treepr;
with Ttypes;
with Types;    use Types;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Urealp;
with Usage;
with Validsw;  use Validsw;

with System.Assertions;

procedure Gnat1drv is
   Main_Unit_Node : Node_Id;
   --  Compilation unit node for main unit

   Main_Kind : Node_Kind;
   --  Kind of main compilation unit node

   Back_End_Mode : Back_End.Back_End_Mode_Type;
   --  Record back end mode

   procedure Adjust_Global_Switches;
   --  There are various interactions between front end switch settings,
   --  including debug switch settings and target dependent parameters.
   --  This procedure takes care of properly handling these interactions.
   --  We do it after scanning out all the switches, so that we are not
   --  depending on the order in which switches appear.

   procedure Check_Bad_Body;
   --  Called to check if the unit we are compiling has a bad body

   procedure Check_Rep_Info;
   --  Called when we are not generating code, to check if -gnatR was requested
   --  and if so, explain that we will not be honoring the request.

   ----------------------------
   -- Adjust_Global_Switches --
   ----------------------------

   procedure Adjust_Global_Switches is
   begin
      --  -gnatd.M enables Relaxed_RM_Semantics

      if Debug_Flag_Dot_MM then
         Relaxed_RM_Semantics := True;
      end if;

      --  -gnatd.V enables special C expansion mode

      if Debug_Flag_Dot_VV then
         Modify_Tree_For_C := True;
      end if;

      --  -gnatd.E sets Error_To_Warning mode, causing selected error messages
      --  to be treated as warnings instead of errors.

      if Debug_Flag_Dot_EE then
         Error_To_Warning := True;
      end if;

      --  Disable CodePeer_Mode in Check_Syntax, since we need front-end
      --  expansion.

      if Operating_Mode = Check_Syntax then
         CodePeer_Mode := False;
      end if;

      --  Set ASIS mode if -gnatt and -gnatc are set

      if Operating_Mode = Check_Semantics and then Tree_Output then
         ASIS_Mode := True;

         --  Turn off inlining in ASIS mode, since ASIS cannot handle the extra
         --  information in the trees caused by inlining being active.

         --  More specifically, the tree seems to be malformed from the ASIS
         --  point of view if -gnatc and -gnatn appear together???

         Inline_Active := False;

         --  Turn off SCIL generation and CodePeer mode in semantics mode,
         --  since SCIL requires front-end expansion.

         Generate_SCIL := False;
         CodePeer_Mode := False;
      end if;

      --  SCIL mode needs to disable front-end inlining since the generated
      --  trees (in particular order and consistency between specs compiled
      --  as part of a main unit or as part of a with-clause) are causing
      --  troubles.

      if Generate_SCIL then
         Front_End_Inlining := False;
      end if;

      --  Tune settings for optimal SCIL generation in CodePeer mode

      if CodePeer_Mode then

         --  Turn off inlining, confuses CodePeer output and gains nothing

         Front_End_Inlining := False;
         Inline_Active      := False;

         --  Disable front-end optimizations, to keep the tree as close to the
         --  source code as possible, and also to avoid inconsistencies between
         --  trees when using different optimization switches.

         Optimization_Level := 0;

         --  Enable some restrictions systematically to simplify the generated
         --  code (and ease analysis). Note that restriction checks are also
         --  disabled in CodePeer mode, see Restrict.Check_Restriction, and
         --  user specified Restrictions pragmas are ignored, see
         --  Sem_Prag.Process_Restrictions_Or_Restriction_Warnings.

         Restrict.Restrictions.Set   (No_Initialize_Scalars)           := True;
         Restrict.Restrictions.Set   (No_Task_Hierarchy)               := True;
         Restrict.Restrictions.Set   (No_Abort_Statements)             := True;
         Restrict.Restrictions.Set   (Max_Asynchronous_Select_Nesting) := True;
         Restrict.Restrictions.Value (Max_Asynchronous_Select_Nesting) := 0;

         --  Suppress division by zero and access checks since they are handled
         --  implicitly by CodePeer.

         --  Turn off dynamic elaboration checks: generates inconsistencies in
         --  trees between specs compiled as part of a main unit or as part of
         --  a with-clause.

         --  Turn off alignment checks: these cannot be proved statically by
         --  CodePeer and generate false positives.

         --  Enable all other language checks

         Suppress_Options.Suppress :=
           (Access_Check      => True,
            Alignment_Check   => True,
            Division_Check    => True,
            Elaboration_Check => True,
            others            => False);

         Dynamic_Elaboration_Checks := False;

         --  Set STRICT mode for overflow checks if not set explicitly. This
         --  prevents suppressing of overflow checks by default, in code down
         --  below.

         if Suppress_Options.Overflow_Mode_General = Not_Set then
            Suppress_Options.Overflow_Mode_General    := Strict;
            Suppress_Options.Overflow_Mode_Assertions := Strict;
         end if;

         --  CodePeer handles division and overflow checks directly, based on
         --  the marks set by the frontend, hence no special expansion should
         --  be performed in the frontend for division and overflow checks.

         Backend_Divide_Checks_On_Target   := True;
         Backend_Overflow_Checks_On_Target := True;

         --  Kill debug of generated code, since it messes up sloc values

         Debug_Generated_Code := False;

         --  Turn cross-referencing on in case it was disabled (e.g. by -gnatD)
         --  Do we really need to spend time generating xref in CodePeer
         --  mode??? Consider setting Xref_Active to False.

         Xref_Active := True;

         --  Polling mode forced off, since it generates confusing junk

         Polling_Required := False;

         --  Set operating mode to Generate_Code to benefit from full front-end
         --  expansion (e.g. generics).

         Operating_Mode := Generate_Code;

         --  We need SCIL generation of course

         Generate_SCIL := True;

         --  Enable assertions, since they give CodePeer valuable extra info

         Assertions_Enabled := True;

         --  Disable all simple value propagation. This is an optimization
         --  which is valuable for code optimization, and also for generation
         --  of compiler warnings, but these are being turned off by default,
         --  and CodePeer generates better messages (referencing original
         --  variables) this way.

         Debug_Flag_MM := True;

         --  Set normal RM validity checking, and checking of IN OUT parameters
         --  (this might give CodePeer more useful checks to analyze, to be
         --  confirmed???). All other validity checking is turned off, since
         --  this can generate very complex trees that only confuse CodePeer
         --  and do not bring enough useful info.

         Reset_Validity_Check_Options;
         Validity_Check_Default       := True;
         Validity_Check_In_Out_Params := True;
         Validity_Check_In_Params     := True;

         --  Turn off style check options since we are not interested in any
         --  front-end warnings when we are getting CodePeer output.

         Reset_Style_Check_Options;

         --  Always perform semantics and generate ali files in CodePeer mode,
         --  so that a gnatmake -c -k will proceed further when possible.

         Force_ALI_Tree_File := True;
         Try_Semantics := True;

         --  Make the Ada front-end more liberal so that the compiler will
         --  allow illegal code that is allowed by other compilers. CodePeer
         --  is in the business of finding problems, not enforcing rules.
         --  This is useful when using CodePeer mode with other compilers.

         Relaxed_RM_Semantics := True;
      end if;

      --  Enable some individual switches that are implied by relaxed RM
      --  semantics mode.

      if Relaxed_RM_Semantics then
         Opt.Allow_Integer_Address := True;
         Overriding_Renamings := True;
         Treat_Categorization_Errors_As_Warnings := True;
      end if;

      --  Enable GNATprove_Mode when using -gnatd.F switch

      if Debug_Flag_Dot_FF then
         GNATprove_Mode := True;
      end if;

      --  GNATprove_Mode is also activated by default in the gnat2why
      --  executable.

      if GNATprove_Mode then

         --  Turn off inlining, which would confuse formal verification output
         --  and gain nothing.

         Front_End_Inlining := False;
         Inline_Active      := False;

         --  Disable front-end optimizations, to keep the tree as close to the
         --  source code as possible, and also to avoid inconsistencies between
         --  trees when using different optimization switches.

         Optimization_Level := 0;

         --  Enable some restrictions systematically to simplify the generated
         --  code (and ease analysis). Note that restriction checks are also
         --  disabled in SPARK mode, see Restrict.Check_Restriction, and user
         --  specified Restrictions pragmas are ignored, see
         --  Sem_Prag.Process_Restrictions_Or_Restriction_Warnings.

         Restrict.Restrictions.Set (No_Initialize_Scalars) := True;

         --  Note: at this point we used to suppress various checks, but that
         --  is not what we want. We need the semantic processing for these
         --  checks (which will set flags like Do_Overflow_Check, showing the
         --  points at which potential checks are required semantically). We
         --  don't want the expansion associated with these checks, but that
         --  happens anyway because this expansion is simply not done in the
         --  SPARK version of the expander.

         --  Turn off dynamic elaboration checks: generates inconsistencies in
         --  trees between specs compiled as part of a main unit or as part of
         --  a with-clause.

         --  Comment is incomplete, SPARK semantics rely on static mode no???

         Dynamic_Elaboration_Checks := False;

         --  Set STRICT mode for overflow checks if not set explicitly. This
         --  prevents suppressing of overflow checks by default, in code down
         --  below.

         if Suppress_Options.Overflow_Mode_General = Not_Set then
            Suppress_Options.Overflow_Mode_General    := Strict;
            Suppress_Options.Overflow_Mode_Assertions := Strict;
         end if;

         --  Kill debug of generated code, since it messes up sloc values

         Debug_Generated_Code := False;

         --  Turn cross-referencing on in case it was disabled (e.g. by -gnatD)
         --  as it is needed for computing effects of subprograms in the formal
         --  verification backend.

         Xref_Active := True;

         --  Polling mode forced off, since it generates confusing junk

         Polling_Required := False;

         --  Set operating mode to Check_Semantics, but a light front-end
         --  expansion is still performed.

         Operating_Mode := Check_Semantics;

         --  Enable assertions, since they give valuable extra information for
         --  formal verification.

         Assertions_Enabled := True;

         --  Turn off style check options since we are not interested in any
         --  front-end warnings when we are getting SPARK output.

         Reset_Style_Check_Options;

         --  Suppress the generation of name tables for enumerations, which are
         --  not needed for formal verification, and fall outside the SPARK
         --  subset (use of pointers).

         Global_Discard_Names := True;

         --  Suppress the expansion of tagged types and dispatching calls,
         --  which lead to the generation of non-SPARK code (use of pointers),
         --  which is more complex to formally verify than the original source.

         Tagged_Type_Expansion := False;
      end if;

      --  Set Configurable_Run_Time mode if system.ads flag set or if the
      --  special debug flag -gnatdY is set.

      if Targparm.Configurable_Run_Time_On_Target or Debug_Flag_YY then
         Configurable_Run_Time_Mode := True;
      end if;

      --  Set -gnatR3m mode if debug flag A set

      if Debug_Flag_AA then
         Back_Annotate_Rep_Info := True;
         List_Representation_Info := 1;
         List_Representation_Info_Mechanisms := True;
      end if;

      --  Force Target_Strict_Alignment true if debug flag -gnatd.a is set

      if Debug_Flag_Dot_A then
         Ttypes.Target_Strict_Alignment := True;
      end if;

      --  Increase size of allocated entities if debug flag -gnatd.N is set

      if Debug_Flag_Dot_NN then
         Atree.Num_Extension_Nodes := Atree.Num_Extension_Nodes + 1;
      end if;

      --  Disable static allocation of dispatch tables if -gnatd.t or if layout
      --  is enabled. The front end's layout phase currently treats types that
      --  have discriminant-dependent arrays as not being static even when a
      --  discriminant constraint on the type is static, and this leads to
      --  problems with subtypes of type Ada.Tags.Dispatch_Table_Wrapper. ???

      if Debug_Flag_Dot_T or else Frontend_Layout_On_Target then
         Static_Dispatch_Tables := False;
      end if;

      --  Flip endian mode if -gnatd8 set

      if Debug_Flag_8 then
         Ttypes.Bytes_Big_Endian := not Ttypes.Bytes_Big_Endian;
      end if;

      --  Deal with forcing OpenVMS switches True if debug flag M is set, but
      --  record the setting of Targparm.Open_VMS_On_Target in True_VMS_Target
      --  before doing this, so we know if we are in real OpenVMS or not.

      Opt.True_VMS_Target := Targparm.OpenVMS_On_Target;

      if Debug_Flag_M then
         Targparm.OpenVMS_On_Target := True;
         Hostparm.OpenVMS := True;
      end if;

      --  Activate front end layout if debug flag -gnatdF is set

      if Debug_Flag_FF then
         Targparm.Frontend_Layout_On_Target := True;
      end if;

      --  Set and check exception mechanism

      if Targparm.ZCX_By_Default_On_Target then
         Exception_Mechanism := Back_End_Exceptions;
      end if;

      --  Set proper status for overflow check mechanism

      --  If already set (by -gnato or above in SPARK or CodePeer mode) then we
      --  have nothing to do.

      if Opt.Suppress_Options.Overflow_Mode_General /= Not_Set then
         null;

      --  Otherwise set overflow mode defaults

      else
         --  Otherwise set overflow checks off by default

         Suppress_Options.Suppress (Overflow_Check) := True;

         --  Set appropriate default overflow handling mode. Note: at present
         --  we set STRICT in all three of the following cases. They are
         --  separated because in the future we may make different choices.

         --  By default set STRICT mode if -gnatg in effect

         if GNAT_Mode then
            Suppress_Options.Overflow_Mode_General    := Strict;
            Suppress_Options.Overflow_Mode_Assertions := Strict;

         --  If we have backend divide and overflow checks, then by default
         --  overflow checks are STRICT. Historically this code used to also
         --  activate overflow checks, although no target currently has these
         --  flags set, so this was dead code anyway.

         elsif Targparm.Backend_Divide_Checks_On_Target
           and
             Targparm.Backend_Overflow_Checks_On_Target
         then
            Suppress_Options.Overflow_Mode_General    := Strict;
            Suppress_Options.Overflow_Mode_Assertions := Strict;

         --  Otherwise for now, default is STRICT mode. This may change in the
         --  future, but for now this is the compatible behavior with previous
         --  versions of GNAT.

         else
            Suppress_Options.Overflow_Mode_General    := Strict;
            Suppress_Options.Overflow_Mode_Assertions := Strict;
         end if;
      end if;

      --  Set default for atomic synchronization. As this synchronization
      --  between atomic accesses can be expensive, and not typically needed
      --  on some targets, an optional target parameter can turn the option
      --  off. Note Atomic Synchronization is implemented as check.

      Suppress_Options.Suppress (Atomic_Synchronization) :=
        not Atomic_Sync_Default_On_Target;

      --  Set switch indicating if back end can handle limited types, and
      --  guarantee that no incorrect copies are made (e.g. in the context
      --  of an if or case expression).

      --  Debug flag -gnatd.L decisively sets usage on

      if Debug_Flag_Dot_LL then
         Back_End_Handles_Limited_Types := True;

      --  If no debug flag, usage off for AAMP, VM, SCIL cases

      elsif AAMP_On_Target
        or else VM_Target /= No_VM
        or else Generate_SCIL
      then
         Back_End_Handles_Limited_Types := False;

      --  Otherwise normal gcc back end, for now still turn flag off by
      --  default, since there are unresolved problems in the front end.

      else
         Back_End_Handles_Limited_Types := False;
      end if;

      --  If the inlining level has not been set by the user, compute it from
      --  the optimization level: 1 at -O1/-O2 (and -Os), 2 at -O3 and above.

      if Inline_Level = 0 then
         if Optimization_Level < 3 then
            Inline_Level := 1;
         else
            Inline_Level := 2;
         end if;
      end if;

      --  Output warning if -gnateE specified and cannot be supported

      if Exception_Extra_Info
        and then Restrict.No_Exception_Handlers_Set
      then
         Set_Standard_Error;
         Write_Str
           ("warning: extra exception information (-gnateE) was specified");
         Write_Eol;
         Write_Str
           ("warning: this capability is not available in this configuration");
         Write_Eol;
         Set_Standard_Output;
      end if;

      --  Finally capture adjusted value of Suppress_Options as the initial
      --  value for Scope_Suppress, which will be modified as we move from
      --  scope to scope (by Suppress/Unsuppress/Overflow_Checks pragmas).

      Sem.Scope_Suppress := Opt.Suppress_Options;
   end Adjust_Global_Switches;

   --------------------
   -- Check_Bad_Body --
   --------------------

   procedure Check_Bad_Body is
      Sname   : Unit_Name_Type;
      Src_Ind : Source_File_Index;
      Fname   : File_Name_Type;

      procedure Bad_Body_Error (Msg : String);
      --  Issue message for bad body found

      --------------------
      -- Bad_Body_Error --
      --------------------

      procedure Bad_Body_Error (Msg : String) is
      begin
         Error_Msg_N (Msg, Main_Unit_Node);
         Error_Msg_File_1 := Fname;
         Error_Msg_N ("remove incorrect body in file{!", Main_Unit_Node);
      end Bad_Body_Error;

   --  Start of processing for Check_Bad_Body

   begin
      --  Nothing to do if we are only checking syntax, because we don't know
      --  enough to know if we require or forbid a body in this case.

      if Operating_Mode = Check_Syntax then
         return;
      end if;

      --  Check for body not allowed

      if (Main_Kind = N_Package_Declaration
           and then not Body_Required (Main_Unit_Node))
        or else (Main_Kind = N_Generic_Package_Declaration
                  and then not Body_Required (Main_Unit_Node))
        or else Main_Kind = N_Package_Renaming_Declaration
        or else Main_Kind = N_Subprogram_Renaming_Declaration
        or else Nkind (Original_Node (Unit (Main_Unit_Node)))
                         in N_Generic_Instantiation
      then
         Sname := Unit_Name (Main_Unit);

         --  If we do not already have a body name, then get the body name
         --  (but how can we have a body name here???)

         if not Is_Body_Name (Sname) then
            Sname := Get_Body_Name (Sname);
         end if;

         Fname := Get_File_Name (Sname, Subunit => False);
         Src_Ind := Load_Source_File (Fname);

         --  Case where body is present and it is not a subunit. Exclude the
         --  subunit case, because it has nothing to do with the package we are
         --  compiling. It is illegal for a child unit and a subunit with the
         --  same expanded name (RM 10.2(9)) to appear together in a partition,
         --  but there is nothing to stop a compilation environment from having
         --  both, and the test here simply allows that. If there is an attempt
         --  to include both in a partition, this is diagnosed at bind time. In
         --  Ada 83 mode this is not a warning case.

         --  Note: if weird file names are being used, we can have a situation
         --  where the file name that supposedly contains body in fact contains
         --  a spec, or we can't tell what it contains. Skip the error message
         --  in these cases.

         --  Also ignore body that is nothing but pragma No_Body; (that's the
         --  whole point of this pragma, to be used this way and to cause the
         --  body file to be ignored in this context).

         if Src_Ind /= No_Source_File
           and then Get_Expected_Unit_Type (Fname) = Expect_Body
           and then not Source_File_Is_Subunit (Src_Ind)
           and then not Source_File_Is_No_Body (Src_Ind)
         then
            Errout.Finalize (Last_Call => False);

            Error_Msg_Unit_1 := Sname;

            --  Ada 83 case of a package body being ignored. This is not an
            --  error as far as the Ada 83 RM is concerned, but it is almost
            --  certainly not what is wanted so output a warning. Give this
            --  message only if there were no errors, since otherwise it may
            --  be incorrect (we may have misinterpreted a junk spec as not
            --  needing a body when it really does).

            if Main_Kind = N_Package_Declaration
              and then Ada_Version = Ada_83
              and then Operating_Mode = Generate_Code
              and then Distribution_Stub_Mode /= Generate_Caller_Stub_Body
              and then not Compilation_Errors
            then
               Error_Msg_N
                 ("package $$ does not require a body??", Main_Unit_Node);
               Error_Msg_File_1 := Fname;
               Error_Msg_N ("body in file{ will be ignored??", Main_Unit_Node);

               --  Ada 95 cases of a body file present when no body is
               --  permitted. This we consider to be an error.

            else
               --  For generic instantiations, we never allow a body

               if Nkind (Original_Node (Unit (Main_Unit_Node)))
               in N_Generic_Instantiation
               then
                  Bad_Body_Error
                    ("generic instantiation for $$ does not allow a body");

                  --  A library unit that is a renaming never allows a body

               elsif Main_Kind in N_Renaming_Declaration then
                  Bad_Body_Error
                    ("renaming declaration for $$ does not allow a body!");

                  --  Remaining cases are packages and generic packages. Here
                  --  we only do the test if there are no previous errors,
                  --  because if there are errors, they may lead us to
                  --  incorrectly believe that a package does not allow a
                  --  body when in fact it does.

               elsif not Compilation_Errors then
                  if Main_Kind = N_Package_Declaration then
                     Bad_Body_Error
                       ("package $$ does not allow a body!");

                  elsif Main_Kind = N_Generic_Package_Declaration then
                     Bad_Body_Error
                       ("generic package $$ does not allow a body!");
                  end if;
               end if;

            end if;
         end if;
      end if;
   end Check_Bad_Body;

   --------------------
   -- Check_Rep_Info --
   --------------------

   procedure Check_Rep_Info is
   begin
      if List_Representation_Info /= 0
        or else List_Representation_Info_Mechanisms
      then
         Set_Standard_Error;
         Write_Eol;
         Write_Str
           ("cannot generate representation information, no code generated");
         Write_Eol;
         Write_Eol;
         Set_Standard_Output;
      end if;
   end Check_Rep_Info;

--  Start of processing for Gnat1drv

begin
   --  This inner block is set up to catch assertion errors and constraint
   --  errors. Since the code for handling these errors can cause another
   --  exception to be raised (namely Unrecoverable_Error), we need two
   --  nested blocks, so that the outer one handles unrecoverable error.

   begin
      --  Initialize all packages. For the most part, these initialization
      --  calls can be made in any order. Exceptions are as follows:

      --  Lib.Initialize need to be called before Scan_Compiler_Arguments,
      --  because it initializes a table filled by Scan_Compiler_Arguments.

      Osint.Initialize;
      Fmap.Reset_Tables;
      Lib.Initialize;
      Lib.Xref.Initialize;
      Scan_Compiler_Arguments;
      Osint.Add_Default_Search_Dirs;

      Nlists.Initialize;
      Sinput.Initialize;
      Sem.Initialize;
      Exp_CG.Initialize;
      Csets.Initialize;
      Uintp.Initialize;
      Urealp.Initialize;
      Errout.Initialize;
      SCOs.Initialize;
      Snames.Initialize;
      Stringt.Initialize;
      Inline.Initialize;
      Par_SCO.Initialize;
      Sem_Ch8.Initialize;
      Sem_Ch12.Initialize;
      Sem_Ch13.Initialize;
      Sem_Elim.Initialize;
      Sem_Eval.Initialize;
      Sem_Type.Init_Interp_Tables;

      --  Acquire target parameters from system.ads (source of package System)

      declare
         use Sinput;

         S : Source_File_Index;
         N : File_Name_Type;

      begin
         Name_Buffer (1 .. 10) := "system.ads";
         Name_Len := 10;
         N := Name_Find;
         S := Load_Source_File (N);

         if S = No_Source_File then
            Write_Line
              ("fatal error, run-time library not installed correctly");
            Write_Line ("cannot locate file system.ads");
            raise Unrecoverable_Error;

         --  Remember source index of system.ads (which was read successfully)

         else
            System_Source_File_Index := S;
         end if;

         Targparm.Get_Target_Parameters
           (System_Text  => Source_Text  (S),
            Source_First => Source_First (S),
            Source_Last  => Source_Last  (S));

         --  Acquire configuration pragma information from Targparm

         Restrict.Restrictions := Targparm.Restrictions_On_Target;
      end;

      Adjust_Global_Switches;

      --  Output copyright notice if full list mode unless we have a list
      --  file, in which case we defer this so that it is output in the file

      if (Verbose_Mode or else (Full_List and then Full_List_File_Name = null))
        and then not Debug_Flag_7
      then
         Write_Eol;
         Write_Str ("GNAT ");
         Write_Str (Gnat_Version_String);
         Write_Eol;
         Write_Str ("Copyright 1992-" & Current_Year
                    & ", Free Software Foundation, Inc.");
         Write_Eol;
      end if;

      --  Check we do not have more than one source file, this happens only in
      --  the case where the driver is called directly, it cannot happen when
      --  gnat1 is invoked from gcc in the normal case.

      if Osint.Number_Of_Files /= 1 then
         Usage;
         Write_Eol;
         Osint.Fail ("you must provide one source file");

      elsif Usage_Requested then
         Usage;
      end if;

      --  Generate target dependent output file if requested

      if Target_Dependent_Info_Write_Name /= null then
         Set_Targ.Write_Target_Dependent_Values;
      end if;

      --  Call the front end

      Original_Operating_Mode := Operating_Mode;
      Frontend;

      --  Exit with errors if the main source could not be parsed.

      if Sinput.Main_Source_File = No_Source_File then
         Errout.Finalize (Last_Call => True);
         Errout.Output_Messages;
         Exit_Program (E_Errors);
      end if;

      Main_Unit_Node := Cunit (Main_Unit);
      Main_Kind := Nkind (Unit (Main_Unit_Node));
      Check_Bad_Body;

      --  In CodePeer mode we always delete old SCIL files before regenerating
      --  new ones, in case of e.g. errors, and also to remove obsolete scilx
      --  files generated by CodePeer itself.

      if CodePeer_Mode then
         Comperr.Delete_SCIL_Files;
      end if;

      --  Exit if compilation errors detected

      Errout.Finalize (Last_Call => False);

      if Compilation_Errors then
         Treepr.Tree_Dump;
         Sem_Ch13.Validate_Unchecked_Conversions;
         Sem_Ch13.Validate_Address_Clauses;
         Sem_Ch13.Validate_Independence;
         Errout.Output_Messages;
         Namet.Finalize;

         --  Generate ALI file if specially requested

         if Opt.Force_ALI_Tree_File then
            Write_ALI (Object => False);
            Tree_Gen;
         end if;

         Errout.Finalize (Last_Call => True);
         Exit_Program (E_Errors);
      end if;

      --  Set Generate_Code on main unit and its spec. We do this even if are
      --  not generating code, since Lib-Writ uses this to determine which
      --  units get written in the ali file.

      Set_Generate_Code (Main_Unit);

      --  If we have a corresponding spec, and it comes from source or it is
      --  not a generated spec for a child subprogram body, then we need object
      --  code for the spec unit as well.

      if Nkind (Unit (Main_Unit_Node)) in N_Unit_Body
        and then not Acts_As_Spec (Main_Unit_Node)
      then
         if Nkind (Unit (Main_Unit_Node)) = N_Subprogram_Body
           and then not Comes_From_Source (Library_Unit (Main_Unit_Node))
         then
            null;
         else
            Set_Generate_Code
              (Get_Cunit_Unit_Number (Library_Unit (Main_Unit_Node)));
         end if;
      end if;

      --  Case of no code required to be generated, exit indicating no error

      if Original_Operating_Mode = Check_Syntax then
         Treepr.Tree_Dump;
         Errout.Finalize (Last_Call => True);
         Errout.Output_Messages;
         Tree_Gen;
         Namet.Finalize;
         Check_Rep_Info;

         --  Use a goto instead of calling Exit_Program so that finalization
         --  occurs normally.

         goto End_Of_Program;

      elsif Original_Operating_Mode = Check_Semantics then
         Back_End_Mode := Declarations_Only;

      --  All remaining cases are cases in which the user requested that code
      --  be generated (i.e. no -gnatc or -gnats switch was used). Check if we
      --  can in fact satisfy this request.

      --  Cannot generate code if someone has turned off code generation for
      --  any reason at all. We will try to figure out a reason below.

      elsif Operating_Mode /= Generate_Code then
         Back_End_Mode := Skip;

      --  We can generate code for a subprogram body unless there were missing
      --  subunits. Note that we always generate code for all generic units (a
      --  change from some previous versions of GNAT).

      elsif Main_Kind = N_Subprogram_Body and then not Subunits_Missing then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a package body unless there are subunits
      --  missing (note that we always generate code for generic units, which
      --  is a change from some earlier versions of GNAT).

      elsif Main_Kind = N_Package_Body and then not Subunits_Missing then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a package declaration or a subprogram
      --  declaration only if it does not required a body.

      elsif Nkind_In (Main_Kind,
              N_Package_Declaration,
              N_Subprogram_Declaration)
        and then
          (not Body_Required (Main_Unit_Node)
             or else
           Distribution_Stub_Mode = Generate_Caller_Stub_Body)
      then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a generic package declaration of a generic
      --  subprogram declaration only if does not require a body.

      elsif Nkind_In (Main_Kind, N_Generic_Package_Declaration,
                                 N_Generic_Subprogram_Declaration)
        and then not Body_Required (Main_Unit_Node)
      then
         Back_End_Mode := Generate_Object;

      --  Compilation units that are renamings do not require bodies, so we can
      --  generate code for them.

      elsif Nkind_In (Main_Kind, N_Package_Renaming_Declaration,
                                 N_Subprogram_Renaming_Declaration)
      then
         Back_End_Mode := Generate_Object;

      --  Compilation units that are generic renamings do not require bodies
      --  so we can generate code for them.

      elsif Main_Kind in N_Generic_Renaming_Declaration then
         Back_End_Mode := Generate_Object;

      --  It is not an error to analyze in CodePeer mode a spec which requires
      --  a body, in order to generate SCIL for this spec.

      elsif CodePeer_Mode then
         Back_End_Mode := Generate_Object;

      --  It is not an error to analyze in GNATprove mode a spec which requires
      --  a body, when the body is not available. During frame condition
      --  generation, the corresponding ALI file is generated. During
      --  analysis, the spec is analyzed.

      elsif GNATprove_Mode then
         Back_End_Mode := Declarations_Only;

      --  In all other cases (specs which have bodies, generics, and bodies
      --  where subunits are missing), we cannot generate code and we generate
      --  a warning message. Note that generic instantiations are gone at this
      --  stage since they have been replaced by their instances.

      else
         Back_End_Mode := Skip;
      end if;

      --  At this stage Back_End_Mode is set to indicate if the backend should
      --  be called to generate code. If it is Skip, then code generation has
      --  been turned off, even though code was requested by the original
      --  command. This is not an error from the user point of view, but it is
      --  an error from the point of view of the gcc driver, so we must exit
      --  with an error status.

      --  We generate an informative message (from the gcc point of view, it
      --  is an error message, but from the users point of view this is not an
      --  error, just a consequence of compiling something that cannot
      --  generate code).

      if Back_End_Mode = Skip then
         Set_Standard_Error;
         Write_Str ("cannot generate code for ");
         Write_Str ("file ");
         Write_Name (Unit_File_Name (Main_Unit));

         if Subunits_Missing then
            Write_Str (" (missing subunits)");
            Write_Eol;

            --  Force generation of ALI file, for backward compatibility

            Opt.Force_ALI_Tree_File := True;

         elsif Main_Kind = N_Subunit then
            Write_Str (" (subunit)");
            Write_Eol;

            --  Force generation of ALI file, for backward compatibility

            Opt.Force_ALI_Tree_File := True;

         elsif Main_Kind = N_Subprogram_Declaration then
            Write_Str (" (subprogram spec)");
            Write_Eol;

         --  Generic package body in GNAT implementation mode

         elsif Main_Kind = N_Package_Body and then GNAT_Mode then
            Write_Str (" (predefined generic)");
            Write_Eol;

            --  Force generation of ALI file, for backward compatibility

            Opt.Force_ALI_Tree_File := True;

         --  Only other case is a package spec

         else
            Write_Str (" (package spec)");
            Write_Eol;
         end if;

         Set_Standard_Output;

         Sem_Ch13.Validate_Unchecked_Conversions;
         Sem_Ch13.Validate_Address_Clauses;
         Sem_Ch13.Validate_Independence;
         Errout.Finalize (Last_Call => True);
         Errout.Output_Messages;
         Treepr.Tree_Dump;
         Tree_Gen;

         --  Generate ALI file if specially requested, or for missing subunits,
         --  subunits or predefined generic.

         if Opt.Force_ALI_Tree_File then
            Write_ALI (Object => False);
         end if;

         Namet.Finalize;
         Check_Rep_Info;

         --  Exit program with error indication, to kill object file

         Exit_Program (E_No_Code);
      end if;

      --  In -gnatc mode, we only do annotation if -gnatt or -gnatR is also set
      --  as indicated by Back_Annotate_Rep_Info being set to True.

      --  We don't call for annotations on a subunit, because to process those
      --  the back-end requires that the parent(s) be properly compiled.

      --  Annotation is suppressed for targets where front-end layout is
      --  enabled, because the front end determines representations.

      --  Annotation is also suppressed in the case of compiling for a VM,
      --  since representations are largely symbolic there.

      if Back_End_Mode = Declarations_Only
        and then
          (not (Back_Annotate_Rep_Info or Generate_SCIL or GNATprove_Mode)
            or else Main_Kind = N_Subunit
            or else Targparm.Frontend_Layout_On_Target
            or else Targparm.VM_Target /= No_VM)
      then
         Sem_Ch13.Validate_Unchecked_Conversions;
         Sem_Ch13.Validate_Address_Clauses;
         Sem_Ch13.Validate_Independence;
         Errout.Finalize (Last_Call => True);
         Errout.Output_Messages;
         Write_ALI (Object => False);
         Tree_Dump;
         Tree_Gen;
         Namet.Finalize;
         Check_Rep_Info;
         return;
      end if;

      --  Ensure that we properly register a dependency on system.ads, since
      --  even if we do not semantically depend on this, Targparm has read
      --  system parameters from the system.ads file.

      Lib.Writ.Ensure_System_Dependency;

      --  Add dependencies, if any, on preprocessing data file and on
      --  preprocessing definition file(s).

      Prepcomp.Add_Dependencies;

      --  Back end needs to explicitly unlock tables it needs to touch

      Atree.Lock;
      Elists.Lock;
      Fname.UF.Lock;
      Inline.Lock;
      Lib.Lock;
      Nlists.Lock;
      Sem.Lock;
      Sinput.Lock;
      Namet.Lock;
      Stringt.Lock;

      --  Here we call the back end to generate the output code

      Generating_Code := True;
      Back_End.Call_Back_End (Back_End_Mode);

      --  Once the backend is complete, we unlock the names table. This call
      --  allows a few extra entries, needed for example for the file name for
      --  the library file output.

      Namet.Unlock;

      --  Generate the call-graph output of dispatching calls

      Exp_CG.Generate_CG_Output;

      --  Validate unchecked conversions (using the values for size and
      --  alignment annotated by the backend where possible).

      Sem_Ch13.Validate_Unchecked_Conversions;

      --  Validate address clauses (again using alignment values annotated
      --  by the backend where possible).

      Sem_Ch13.Validate_Address_Clauses;

      --  Validate independence pragmas (again using values annotated by
      --  the back end for component layout etc.)

      Sem_Ch13.Validate_Independence;

      --  Now we complete output of errors, rep info and the tree info. These
      --  are delayed till now, since it is perfectly possible for gigi to
      --  generate errors, modify the tree (in particular by setting flags
      --  indicating that elaboration is required, and also to back annotate
      --  representation information for List_Rep_Info.

      Errout.Finalize (Last_Call => True);
      Errout.Output_Messages;
      List_Rep_Info (Ttypes.Bytes_Big_Endian);
      List_Inlining_Info;

      --  Only write the library if the backend did not generate any error
      --  messages. Otherwise signal errors to the driver program so that
      --  there will be no attempt to generate an object file.

      if Compilation_Errors then
         Treepr.Tree_Dump;
         Exit_Program (E_Errors);
      end if;

      --  In GNATprove mode, an "object" file is always generated as the
      --  result of calling gnat1 or gnat2why, although this is not the
      --  same as the object file produced for compilation.

      Write_ALI (Object => (Back_End_Mode = Generate_Object
                             or else GNATprove_Mode));

      if not Compilation_Errors then

         --  In case of ada backends, we need to make sure that the generated
         --  object file has a timestamp greater than the ALI file. We do this
         --  to make gnatmake happy when checking the ALI and obj timestamps,
         --  where it expects the object file being written after the ali file.

         --  Gnatmake's assumption is true for gcc platforms where the gcc
         --  wrapper needs to call the assembler after calling gnat1, but is
         --  not true for ada backends, where the object files are created
         --  directly by gnat1 (so are created before the ali file).

         Back_End.Gen_Or_Update_Object_File;
      end if;

      --  Generate ASIS tree after writing the ALI file, since in ASIS mode,
      --  Write_ALI may in fact result in further tree decoration from the
      --  original tree file. Note that we dump the tree just before generating
      --  it, so that the dump will exactly reflect what is written out.

      Treepr.Tree_Dump;
      Tree_Gen;

      --  Finalize name table and we are all done

      Namet.Finalize;

   exception
      --  Handle fatal internal compiler errors

      when Rtsfind.RE_Not_Available =>
         Comperr.Compiler_Abort ("RE_Not_Available");

      when System.Assertions.Assert_Failure =>
         Comperr.Compiler_Abort ("Assert_Failure");

      when Constraint_Error =>
         Comperr.Compiler_Abort ("Constraint_Error");

      when Program_Error =>
         Comperr.Compiler_Abort ("Program_Error");

      when Storage_Error =>

         --  Assume this is a bug. If it is real, the message will in any case
         --  say Storage_Error, giving a strong hint.

         Comperr.Compiler_Abort ("Storage_Error");
   end;

   <<End_Of_Program>>
   null;

   --  The outer exception handles an unrecoverable error

exception
   when Unrecoverable_Error =>
      Errout.Finalize (Last_Call => True);
      Errout.Output_Messages;

      Set_Standard_Error;
      Write_Str ("compilation abandoned");
      Write_Eol;

      Set_Standard_Output;
      Source_Dump;
      Tree_Dump;
      Exit_Program (E_Errors);

end Gnat1drv;
