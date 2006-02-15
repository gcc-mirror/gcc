------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             G N A T 1 D R V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2006 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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
with Prepcomp;
with Repinfo;  use Repinfo;
with Restrict;
with Sem;
with Sem_Ch8;
with Sem_Ch12;
with Sem_Ch13;
with Sem_Elim;
with Sem_Eval;
with Sem_Type;
with Sinfo;    use Sinfo;
with Sinput.L; use Sinput.L;
with Snames;
with Sprint;   use Sprint;
with Stringt;
with Targparm;
with Tree_Gen;
with Treepr;   use Treepr;
with Ttypes;
with Types;    use Types;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Urealp;
with Usage;

with System.Assertions;

procedure Gnat1drv is
   Main_Unit_Node : Node_Id;
   --  Compilation unit node for main unit

   Main_Kind : Node_Kind;
   --  Kind of main compilation unit node

   Back_End_Mode : Back_End.Back_End_Mode_Type;
   --  Record back end mode

begin
   --  This inner block is set up to catch assertion errors and constraint
   --  errors. Since the code for handling these errors can cause another
   --  exception to be raised (namely Unrecoverable_Error), we need two
   --  nested blocks, so that the outer one handles unrecoverable error.

   begin
      --  Lib.Initialize need to be called before Scan_Compiler_Arguments,
      --  because it initialize a table that is filled by
      --  Scan_Compiler_Arguments.

      Osint.Initialize;
      Fmap.Reset_Tables;
      Lib.Initialize;
      Lib.Xref.Initialize;
      Scan_Compiler_Arguments;
      Osint.Add_Default_Search_Dirs;

      Nlists.Initialize;
      Sinput.Initialize;
      Sem.Initialize;
      Csets.Initialize;
      Uintp.Initialize;
      Urealp.Initialize;
      Errout.Initialize;
      Namet.Initialize;
      Snames.Initialize;
      Stringt.Initialize;
      Inline.Initialize;
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
         N : Name_Id;

      begin
         Name_Buffer (1 .. 10) := "system.ads";
         Name_Len := 10;
         N := Name_Find;
         S := Load_Source_File (N);

         if S = No_Source_File then
            Write_Line
              ("fatal error, run-time library not installed correctly");
            Write_Line
              ("cannot locate file system.ads");
            raise Unrecoverable_Error;

         --  Remember source index of system.ads (which was read successfully)

         else
            System_Source_File_Index := S;
         end if;

         Targparm.Get_Target_Parameters
           (System_Text  => Source_Text (S),
            Source_First => Source_First (S),
            Source_Last  => Source_Last (S));

         --  Acquire configuration pragma information from Targparm

         Restrict.Restrictions := Targparm.Restrictions_On_Target;
      end;

      --  Set Configurable_Run_Time mode if system.ads flag set

      if Targparm.Configurable_Run_Time_On_Target or Debug_Flag_YY then
         Configurable_Run_Time_Mode := True;
      end if;

      --  Set -gnatR3m mode if debug flag A set

      if Debug_Flag_AA then
         Back_Annotate_Rep_Info := True;
         List_Representation_Info := 1;
         List_Representation_Info_Mechanisms := True;
      end if;

      --  Output copyright notice if full list mode

      if (Verbose_Mode or Full_List)
        and then (not Debug_Flag_7)
      then
         Write_Eol;
         Write_Str ("GNAT ");
         Write_Str (Gnat_Version_String);
         Write_Eol;
         Write_Str ("Copyright 1992-" &
                    Current_Year &
                    ", Free Software Foundation, Inc.");
         Write_Eol;
      end if;

      --  Before we do anything else, adjust certain global values for
      --  debug switches which modify their normal natural settings.

      if Debug_Flag_8 then
         Ttypes.Bytes_Big_Endian := not Ttypes.Bytes_Big_Endian;
      end if;

      if Debug_Flag_M then
         Targparm.OpenVMS_On_Target := True;
         Hostparm.OpenVMS := True;
      end if;

      if Debug_Flag_FF then
         Targparm.Frontend_Layout_On_Target := True;
      end if;

      --  We take the default exception mechanism into account

      if Targparm.ZCX_By_Default_On_Target then
         if Targparm.GCC_ZCX_Support_On_Target then
            Exception_Mechanism := Back_End_Exceptions;
         else
            Osint.Fail
              ("Zero Cost Exceptions not supported on this target");
         end if;
      end if;

      --  Set proper status for overflow checks. We turn on overflow checks
      --  if -gnatp was not specified, and either -gnato is set or the back
      --  end takes care of overflow checks. Otherwise we suppress overflow
      --  checks by default (since front end checks are expensive).

      if not Opt.Suppress_Checks
        and then (Opt.Enable_Overflow_Checks
                    or else
                      (Targparm.Backend_Divide_Checks_On_Target
                        and
                       Targparm.Backend_Overflow_Checks_On_Target))
      then
         Suppress_Options (Overflow_Check) := False;
      else
         Suppress_Options (Overflow_Check) := True;
      end if;

      --  Check we have exactly one source file, this happens only in the case
      --  where the driver is called directly, it cannot happen when gnat1 is
      --  invoked from gcc in the normal case.

      if Osint.Number_Of_Files /= 1 then
         Usage;
         Write_Eol;
         Osint.Fail ("you must provide one source file");

      elsif Usage_Requested then
         Usage;
      end if;

      Original_Operating_Mode := Operating_Mode;
      Frontend;
      Main_Unit_Node := Cunit (Main_Unit);
      Main_Kind := Nkind (Unit (Main_Unit_Node));

      --  Check for suspicious or incorrect body present if we are doing
      --  semantic checking. We omit this check in syntax only mode, because
      --  in that case we do not know if we need a body or not.

      if Operating_Mode /= Check_Syntax
        and then
          ((Main_Kind = N_Package_Declaration
             and then not Body_Required (Main_Unit_Node))
           or else (Main_Kind = N_Generic_Package_Declaration
                     and then not Body_Required (Main_Unit_Node))
           or else Main_Kind = N_Package_Renaming_Declaration
           or else Main_Kind = N_Subprogram_Renaming_Declaration
           or else Nkind (Original_Node (Unit (Main_Unit_Node)))
                           in N_Generic_Instantiation)
      then
         Bad_Body : declare
            Sname   : Unit_Name_Type := Unit_Name (Main_Unit);
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
               Error_Msg_Name_1 := Fname;
               Error_Msg_N
                 ("remove incorrect body in file{!", Main_Unit_Node);
            end Bad_Body_Error;

         --  Start of processing for Bad_Body

         begin
            Sname := Unit_Name (Main_Unit);

            --  If we do not already have a body name, then get the body name
            --  (but how can we have a body name here ???)

            if not Is_Body_Name (Sname) then
               Sname := Get_Body_Name (Sname);
            end if;

            Fname := Get_File_Name (Sname, Subunit => False);
            Src_Ind := Load_Source_File (Fname);

            --  Case where body is present and it is not a subunit. Exclude
            --  the subunit case, because it has nothing to do with the
            --  package we are compiling. It is illegal for a child unit and a
            --  subunit with the same expanded name (RM 10.2(9)) to appear
            --  together in a partition, but there is nothing to stop a
            --  compilation environment from having both, and the test here
            --  simply allows that. If there is an attempt to include both in
            --  a partition, this is diagnosed at bind time. In Ada 83 mode
            --  this is not a warning case.

            --  Note: if weird file names are being used, we can have
            --  situation where the file name that supposedly contains body,
            --  in fact contains a spec, or we can't tell what it contains.
            --  Skip the error message in these cases.

            if Src_Ind /= No_Source_File
              and then Get_Expected_Unit_Type (Fname) = Expect_Body
              and then not Source_File_Is_Subunit (Src_Ind)
            then
               Error_Msg_Name_1 := Sname;

               --  Ada 83 case of a package body being ignored. This is not an
               --  error as far as the Ada 83 RM is concerned, but it is
               --  almost certainly not what is wanted so output a warning.
               --  Give this message only if there were no errors, since
               --  otherwise it may be incorrect (we may have misinterpreted a
               --  junk spec as not needing a body when it really does).

               if Main_Kind = N_Package_Declaration
                 and then Ada_Version = Ada_83
                 and then Operating_Mode = Generate_Code
                 and then Distribution_Stub_Mode /= Generate_Caller_Stub_Body
                 and then not Compilation_Errors
               then
                  Error_Msg_N
                    ("package % does not require a body?", Main_Unit_Node);
                  Error_Msg_Name_1 := Fname;
                  Error_Msg_N
                    ("body in file{? will be ignored", Main_Unit_Node);

               --  Ada 95 cases of a body file present when no body is
               --  permitted. This we consider to be an error.

               else
                  --  For generic instantiations, we never allow a body

                  if Nkind (Original_Node (Unit (Main_Unit_Node)))
                      in N_Generic_Instantiation
                  then
                     Bad_Body_Error
                       ("generic instantiation for % does not allow a body");

                  --  A library unit that is a renaming never allows a body

                  elsif Main_Kind in N_Renaming_Declaration then
                     Bad_Body_Error
                       ("renaming declaration for % does not allow a body!");

                  --  Remaining cases are packages and generic packages. Here
                  --  we only do the test if there are no previous errors,
                  --  because if there are errors, they may lead us to
                  --  incorrectly believe that a package does not allow a body
                  --  when in fact it does.

                  elsif not Compilation_Errors then
                     if Main_Kind = N_Package_Declaration then
                        Bad_Body_Error
                          ("package % does not allow a body!");

                     elsif Main_Kind = N_Generic_Package_Declaration then
                        Bad_Body_Error
                          ("generic package % does not allow a body!");
                     end if;
                  end if;

               end if;
            end if;
         end Bad_Body;
      end if;

      --  Exit if compilation errors detected

      if Compilation_Errors then
         Treepr.Tree_Dump;
         Sem_Ch13.Validate_Unchecked_Conversions;
         Errout.Finalize;
         Namet.Finalize;

         --  Generate ALI file if specially requested

         if Opt.Force_ALI_Tree_File then
            Write_ALI (Object => False);
            Tree_Gen;
         end if;

         Exit_Program (E_Errors);
      end if;

      --  Set Generate_Code on main unit and its spec. We do this even if are
      --  not generating code, since Lib-Writ uses this to determine which
      --  units get written in the ali file.

      Set_Generate_Code (Main_Unit);

      --  If we have a corresponding spec, then we need object
      --  code for the spec unit as well

      if Nkind (Unit (Main_Unit_Node)) in N_Unit_Body
        and then not Acts_As_Spec (Main_Unit_Node)
      then
         Set_Generate_Code
           (Get_Cunit_Unit_Number (Library_Unit (Main_Unit_Node)));
      end if;

      --  Case of no code required to be generated, exit indicating no error

      if Original_Operating_Mode = Check_Syntax then
         Treepr.Tree_Dump;
         Errout.Finalize;
         Tree_Gen;
         Namet.Finalize;

         --  Use a goto instead of calling Exit_Program so that finalization
         --  occurs normally.

         goto End_Of_Program;

      elsif Original_Operating_Mode = Check_Semantics then
         Back_End_Mode := Declarations_Only;

      --  All remaining cases are cases in which the user requested that code
      --  be generated (i.e. no -gnatc or -gnats switch was used). Check if
      --  we can in fact satisfy this request.

      --  Cannot generate code if someone has turned off code generation for
      --  any reason at all. We will try to figure out a reason below.

      elsif Operating_Mode /= Generate_Code then
         Back_End_Mode := Skip;

      --  We can generate code for a subprogram body unless there were missing
      --  subunits. Note that we always generate code for all generic units (a
      --  change from some previous versions of GNAT).

      elsif Main_Kind = N_Subprogram_Body
        and then not Subunits_Missing
      then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a package body unless there are subunits
      --  missing (note that we always generate code for generic units, which
      --  is a change from some earlier versions of GNAT).

      elsif Main_Kind = N_Package_Body
        and then not Subunits_Missing
      then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a package declaration or a subprogram
      --  declaration only if it does not required a body.

      elsif (Main_Kind = N_Package_Declaration
               or else
             Main_Kind = N_Subprogram_Declaration)
        and then
          (not Body_Required (Main_Unit_Node)
             or else
           Distribution_Stub_Mode = Generate_Caller_Stub_Body)
      then
         Back_End_Mode := Generate_Object;

      --  We can generate code for a generic package declaration of a generic
      --  subprogram declaration only if does not require a body.

      elsif (Main_Kind = N_Generic_Package_Declaration
               or else
             Main_Kind = N_Generic_Subprogram_Declaration)
        and then not Body_Required (Main_Unit_Node)
      then
         Back_End_Mode := Generate_Object;

      --  Compilation units that are renamings do not require bodies,
      --  so we can generate code for them.

      elsif Main_Kind = N_Package_Renaming_Declaration
        or else Main_Kind = N_Subprogram_Renaming_Declaration
      then
         Back_End_Mode := Generate_Object;

      --  Compilation units that are generic renamings do not require bodies
      --  so we can generate code for them.

      elsif Main_Kind in N_Generic_Renaming_Declaration then
         Back_End_Mode := Generate_Object;

      --  In all other cases (specs which have bodies, generics, and bodies
      --  where subunits are missing), we cannot generate code and we generate
      --  a warning message. Note that generic instantiations are gone at this
      --  stage since they have been replaced by their instances.

      else
         Back_End_Mode := Skip;
      end if;

      --  At this stage Call_Back_End is set to indicate if the backend should
      --  be called to generate code. If it is not set, then code generation
      --  has been turned off, even though code was requested by the original
      --  command. This is not an error from the user point of view, but it is
      --  an error from the point of view of the gcc driver, so we must exit
      --  with an error status.

      --  We generate an informative message (from the gcc point of view, it
      --  is an error message, but from the users point of view this is not an
      --  error, just a consequence of compiling something that cannot
      --  generate code).

      if Back_End_Mode = Skip then
         Write_Str ("cannot generate code for ");
         Write_Str ("file ");
         Write_Name (Unit_File_Name (Main_Unit));

         if Subunits_Missing then
            Write_Str (" (missing subunits)");
            Write_Eol;
            Write_Str ("to check parent unit");

         elsif Main_Kind = N_Subunit then
            Write_Str (" (subunit)");
            Write_Eol;
            Write_Str ("to check subunit");

         elsif Main_Kind = N_Subprogram_Declaration then
            Write_Str (" (subprogram spec)");
            Write_Eol;
            Write_Str ("to check subprogram spec");

         --  Generic package body in GNAT implementation mode

         elsif Main_Kind = N_Package_Body and then GNAT_Mode then
            Write_Str (" (predefined generic)");
            Write_Eol;
            Write_Str ("to check predefined generic");

         --  Only other case is a package spec

         else
            Write_Str (" (package spec)");
            Write_Eol;
            Write_Str ("to check package spec");
         end if;

         Write_Str (" for errors, use ");

         if Hostparm.OpenVMS then
            Write_Str ("/NOLOAD");
         else
            Write_Str ("-gnatc");
         end if;

         Write_Eol;

         Sem_Ch13.Validate_Unchecked_Conversions;
         Errout.Finalize;
         Treepr.Tree_Dump;
         Tree_Gen;
         Write_ALI (Object => False);
         Namet.Finalize;

         --  Exit program with error indication, to kill object file

         Exit_Program (E_No_Code);
      end if;

      --  In -gnatc mode, we only do annotation if -gnatt or -gnatR is also
      --  set as indicated by Back_Annotate_Rep_Info being set to True.

      --  We don't call for annotations on a subunit, because to process those
      --  the back-end requires that the parent(s) be properly compiled.

      --  Annotation is suppressed for targets where front-end layout is
      --  enabled, because the front end determines representations.

      --  Annotation is also suppressed in the case of compiling for
      --  the Java VM, since representations are largely symbolic there.

      if Back_End_Mode = Declarations_Only
        and then (not Back_Annotate_Rep_Info
                    or else Main_Kind = N_Subunit
                    or else Targparm.Frontend_Layout_On_Target
                    or else Hostparm.Java_VM)
      then
         Sem_Ch13.Validate_Unchecked_Conversions;
         Errout.Finalize;
         Write_ALI (Object => False);
         Tree_Dump;
         Tree_Gen;
         Namet.Finalize;
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

      Back_End.Call_Back_End (Back_End_Mode);

      --  Once the backend is complete, we unlock the names table. This call
      --  allows a few extra entries, needed for example for the file name for
      --  the library file output.

      Namet.Unlock;

      --  Validate unchecked conversions (using the values for size and
      --  alignment annotated by the backend where possible).

      Sem_Ch13.Validate_Unchecked_Conversions;

      --  Now we complete output of errors, rep info and the tree info. These
      --  are delayed till now, since it is perfectly possible for gigi to
      --  generate errors, modify the tree (in particular by setting flags
      --  indicating that elaboration is required, and also to back annotate
      --  representation information for List_Rep_Info.

      Errout.Finalize;
      List_Rep_Info;

      --  Only write the library if the backend did not generate any error
      --  messages. Otherwise signal errors to the driver program so that
      --  there will be no attempt to generate an object file.

      if Compilation_Errors then
         Treepr.Tree_Dump;
         Exit_Program (E_Errors);
      end if;

      Write_ALI (Object => (Back_End_Mode = Generate_Object));

      --  Generate the ASIS tree after writing the ALI file, since in ASIS
      --  mode, Write_ALI may in fact result in further tree decoration from
      --  the original tree file. Note that we dump the tree just before
      --  generating it, so that the dump will exactly reflect what is written
      --  out.

      Treepr.Tree_Dump;
      Tree_Gen;

      --  Finalize name table and we are all done

      Namet.Finalize;

   exception
      --  Handle fatal internal compiler errors

      when System.Assertions.Assert_Failure =>
         Comperr.Compiler_Abort ("Assert_Failure");

      when Constraint_Error =>
         Comperr.Compiler_Abort ("Constraint_Error");

      when Program_Error =>
         Comperr.Compiler_Abort ("Program_Error");

      when Storage_Error =>

         --  Assume this is a bug. If it is real, the message will in any case
         --  say Storage_Error, giving a strong hint!

         Comperr.Compiler_Abort ("Storage_Error");
   end;

   <<End_Of_Program>>
   null;

   --  The outer exception handles an unrecoverable error

exception
   when Unrecoverable_Error =>
      Errout.Finalize;

      Set_Standard_Error;
      Write_Str ("compilation abandoned");
      Write_Eol;

      Set_Standard_Output;
      Source_Dump;
      Tree_Dump;
      Exit_Program (E_Errors);

end Gnat1drv;
