------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              B I N D G E N                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.5.10.1 $
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with ALI;         use ALI;
with Binde;       use Binde;
with Butil;       use Butil;
with Casing;      use Casing;
with Fname;       use Fname;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with Gnatvsn;     use Gnatvsn;
with Hostparm;
with Namet;       use Namet;
with Opt;         use Opt;
with Osint;       use Osint;
with Output;      use Output;
with Types;       use Types;
with Sdefault;    use Sdefault;
with System;      use System;

with GNAT.Heap_Sort_A;     use GNAT.Heap_Sort_A;

package body Bindgen is

   Statement_Buffer : String (1 .. 1000);
   --  Buffer used for constructing output statements

   Last : Natural := 0;
   --  Last location in Statement_Buffer currently set

   With_DECGNAT : Boolean := False;
   --  Flag which indicates whether the program uses the DECGNAT library
   --  (presence of the unit System.Aux_DEC.DECLIB)

   With_GNARL : Boolean := False;
   --  Flag which indicates whether the program uses the GNARL library
   --  (presence of the unit System.OS_Interface)

   Num_Elab_Calls : Nat := 0;
   --  Number of generated calls to elaboration routines

   subtype chars_ptr is Address;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure WBI (Info : String) renames Osint.Write_Binder_Info;
   --  Convenient shorthand used throughout

   function ABE_Boolean_Required (U : Unit_Id) return Boolean;
   --  Given a unit id value U, determines if the corresponding unit requires
   --  an access-before-elaboration check variable, i.e. it is a non-predefined
   --  body for which no pragma Elaborate, Elaborate_All or Elaborate_Body is
   --  present, and thus could require ABE checks.

   procedure Resolve_Binder_Options;
   --  Set the value of With_GNARL and With_DECGNAT. The latter only on VMS
   --  since it tests for a package named "dec" which might cause a conflict
   --  on non-VMS systems.

   procedure Gen_Adainit_Ada;
   --  Generates the Adainit procedure (Ada code case)

   procedure Gen_Adainit_C;
   --  Generates the Adainit procedure (C code case)

   procedure Gen_Adafinal_Ada;
   --  Generate the Adafinal procedure (Ada code case)

   procedure Gen_Adafinal_C;
   --  Generate the Adafinal procedure (C code case)

   procedure Gen_Elab_Calls_Ada;
   --  Generate sequence of elaboration calls (Ada code case)

   procedure Gen_Elab_Calls_C;
   --  Generate sequence of elaboration calls (C code case)

   procedure Gen_Elab_Order_Ada;
   --  Generate comments showing elaboration order chosen (Ada case)

   procedure Gen_Elab_Order_C;
   --  Generate comments showing elaboration order chosen (C case)

   procedure Gen_Elab_Defs_C;
   --  Generate sequence of definitions for elaboration routines (C code case)

   procedure Gen_Exception_Table_Ada;
   --  Generate binder exception table (Ada code case). This consists of
   --  declarations followed by a begin followed by a call. If zero cost
   --  exceptions are not active, then only the begin is generated.

   procedure Gen_Exception_Table_C;
   --  Generate binder exception table (C code case). This has no effect
   --  if zero cost exceptions are not active, otherwise it generates a
   --  set of declarations followed by a call.

   procedure Gen_Main_Ada;
   --  Generate procedure main (Ada code case)

   procedure Gen_Main_C;
   --  Generate main() procedure (C code case)

   procedure Gen_Object_Files_Options;
   --  Output comments containing a list of the full names of the object
   --  files to be linked and the list of linker options supplied by
   --  Linker_Options pragmas in the source. (C and Ada code case)

   procedure Gen_Output_File_Ada (Filename : String);
   --  Generate output file (Ada code case)

   procedure Gen_Output_File_C (Filename : String);
   --  Generate output file (C code case)

   procedure Gen_Scalar_Values;
   --  Generates scalar initialization values for -Snn. A single procedure
   --  handles both the Ada and C cases, since there is much common code.

   procedure Gen_Versions_Ada;
   --  Output series of definitions for unit versions (Ada code case)

   procedure Gen_Versions_C;
   --  Output series of definitions for unit versions (C code case)

   function Get_Ada_Main_Name return String;
   --  This function is used in the Ada main output case to compute a usable
   --  name for the generated main program. The normal main program name is
   --  Ada_Main, but this won't work if the user has a unit with this name.
   --  This function tries Ada_Main first, and if there is such a clash, then
   --  it tries Ada_Name_01, Ada_Name_02 ... Ada_Name_99 in sequence.

   function Get_Main_Name return String;
   --  This function is used in the Ada main output case to compute the
   --  correct external main program. It is "main" by default, except on
   --  VxWorks where it is the name of the Ada main name without the "_ada".
   --  the -Mname binder option overrides the default with name.

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean;
   --  Compare linker options, when sorting, first according to
   --  Is_Internal_File (internal files come later) and then by elaboration
   --  order position (latest to earliest) except its not possible to
   --  distinguish between a linker option in the spec and one in the body.

   procedure Move_Linker_Option (From : Natural; To : Natural);
   --  Move routine for sorting linker options

   procedure Set_Char (C : Character);
   --  Set given character in Statement_Buffer at the Last + 1 position
   --  and increment Last by one to reflect the stored character.

   procedure Set_Int (N : Int);
   --  Set given value in decimal in Statement_Buffer with no spaces
   --  starting at the Last + 1 position, and updating Last past the value.
   --  A minus sign is output for a negative value.

   procedure Set_Main_Program_Name;
   --  Given the main program name in Name_Buffer (length in Name_Len)
   --  generate the name of the routine to be used in the call. The name
   --  is generated starting at Last + 1, and Last is updated past it.

   procedure Set_Name_Buffer;
   --  Set the value stored in positions 1 .. Name_Len of the Name_Buffer.

   procedure Set_String (S : String);
   --  Sets characters of given string in Statement_Buffer, starting at the
   --  Last + 1 position, and updating last past the string value.

   procedure Set_Unit_Name;
   --  Given a unit name in the Name_Buffer, copies it to Statement_Buffer,
   --  starting at the Last + 1 position, and updating last past the value.
   --  changing periods to double underscores, and updating Last appropriately.

   procedure Set_Unit_Number (U : Unit_Id);
   --  Sets unit number (first unit is 1, leading zeroes output to line
   --  up all output unit numbers nicely as required by the value, and
   --  by the total number of units.

   procedure Tab_To (N : Natural);
   --  If Last is greater than or equal to N, no effect, otherwise store
   --  blanks in Statement_Buffer bumping Last, until Last = N.

   function Value (chars : chars_ptr) return String;
   --  Return C NUL-terminated string at chars as an Ada string

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String);
   --  For C code case, write C & Common, for Ada case write Ada & Common
   --  to current binder output file using Write_Binder_Info.

   procedure Write_Statement_Buffer;
   --  Write out contents of statement buffer up to Last, and reset Last to 0

   procedure Write_Statement_Buffer (S : String);
   --  First writes its argument (using Set_String (S)), then writes out the
   --  contents of statement buffer up to Last, and reset Last to 0

   --------------------------
   -- ABE_Boolean_Required --
   --------------------------

   function ABE_Boolean_Required (U : Unit_Id) return Boolean is
      Typ   : constant Unit_Type := Units.Table (U).Utype;
      Unit : Unit_Id;

   begin
      if Typ /= Is_Body then
         return False;

      else
         Unit := U + 1;

         return (not Units.Table (Unit).Pure)
                   and then
                (not Units.Table (Unit).Preelab)
                   and then
                (not Units.Table (Unit).Elaborate_Body)
                   and then
                (not Units.Table (Unit).Predefined);
      end if;
   end ABE_Boolean_Required;

   ----------------------
   -- Gen_Adafinal_Ada --
   ----------------------

   procedure Gen_Adafinal_Ada is
   begin
      WBI ("");
      WBI ("   procedure " & Ada_Final_Name.all & " is");
      WBI ("   begin");

      --  If compiling for the JVM, we directly call Adafinal because
      --  we don't import it via Do_Finalize (see Gen_Output_File_Ada).

      if Hostparm.Java_VM then
         WBI ("      System.Standard_Library.Adafinal;");
      else
         WBI ("      Do_Finalize;");
      end if;

      WBI ("   end " & Ada_Final_Name.all & ";");
   end Gen_Adafinal_Ada;

   --------------------
   -- Gen_Adafinal_C --
   --------------------

   procedure Gen_Adafinal_C is
   begin
      WBI ("void " & Ada_Final_Name.all & " () {");
      WBI ("   system__standard_library__adafinal ();");
      WBI ("}");
      WBI ("");
   end Gen_Adafinal_C;

   ---------------------
   -- Gen_Adainit_Ada --
   ---------------------

   procedure Gen_Adainit_Ada is
      Main_Priority : Int renames ALIs.Table (ALIs.First).Main_Priority;
   begin
      WBI ("   procedure " & Ada_Init_Name.all & " is");

      --  Generate externals for elaboration entities

      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
            U    : Unit_Record renames Units.Table (Unum);

         begin
            if U.Set_Elab_Entity then
               Set_String ("      ");
               Set_String ("E");
               Set_Unit_Number (Unum);
               Set_String (" : Boolean; pragma Import (Ada, ");
               Set_String ("E");
               Set_Unit_Number (Unum);
               Set_String (", """);
               Get_Name_String (U.Uname);

               --  In the case of JGNAT we need to emit an Import name
               --  that includes the class name (using '$' separators
               --  in the case of a child unit name).

               if Hostparm.Java_VM then
                  for J in 1 .. Name_Len - 2 loop
                     if Name_Buffer (J) /= '.' then
                        Set_Char (Name_Buffer (J));
                     else
                        Set_String ("$");
                     end if;
                  end loop;

                  Set_String (".");

                  --  If the unit name is very long, then split the
                  --  Import link name across lines using "&" (occurs
                  --  in some C2 tests).

                  if 2 * Name_Len + 60 > Hostparm.Max_Line_Length then
                     Set_String (""" &");
                     Write_Statement_Buffer;
                     Set_String ("         """);
                  end if;
               end if;

               Set_Unit_Name;
               Set_String ("_E"");");
               Write_Statement_Buffer;
            end if;
         end;
      end loop;

      Write_Statement_Buffer;

      --  Normal case (not No_Run_Time mode). The global values are
      --  assigned using the runtime routine Set_Globals (we have to use
      --  the routine call, rather than define the globals in the binder
      --  file to deal with cross-library calls in some systems.

      if No_Run_Time_Specified then

         --  Case of No_Run_Time mode. The only global variable that might
         --  be needed (by the Ravenscar profile) is the priority of the
         --  environment. Also no exception tables are needed.

         if Main_Priority /= No_Main_Priority then
            WBI ("      Main_Priority : Integer;");
            WBI ("      pragma Import (C, Main_Priority," &
                 " ""__gl_main_priority"");");
            WBI ("");
         end if;

         WBI ("   begin");

         if Main_Priority /= No_Main_Priority then
            Set_String ("      Main_Priority := ");
            Set_Int    (Main_Priority);
            Set_Char   (';');
            Write_Statement_Buffer;

         else
            WBI ("      null;");
         end if;

      else
         WBI ("");
         WBI ("      procedure Set_Globals");
         WBI ("        (Main_Priority            : Integer;");
         WBI ("         Time_Slice_Value         : Integer;");
         WBI ("         WC_Encoding              : Character;");
         WBI ("         Locking_Policy           : Character;");
         WBI ("         Queuing_Policy           : Character;");
         WBI ("         Task_Dispatching_Policy  : Character;");
         WBI ("         Adafinal                 : System.Address;");
         WBI ("         Unreserve_All_Interrupts : Integer;");
         WBI ("         Exception_Tracebacks     : Integer);");
         WBI ("      pragma Import (C, Set_Globals, ""__gnat_set_globals"");");
         WBI ("");

         --  Import entry point for elaboration time signal handler
         --  installation, and indication of whether it's been called
         --  previously
         WBI ("");
         WBI ("      procedure Install_Handler;");
         WBI ("      pragma Import (C, Install_Handler, " &
              """__gnat_install_handler"");");
         WBI ("");
         WBI ("      Handler_Installed : Integer;");
         WBI ("      pragma Import (C, Handler_Installed, " &
              """__gnat_handler_installed"");");

         --  Generate exception table

         Gen_Exception_Table_Ada;

         --  Generate the call to Set_Globals

         WBI ("      Set_Globals");

         Set_String ("        (Main_Priority            => ");
         Set_Int    (Main_Priority);
         Set_Char   (',');
         Write_Statement_Buffer;

         Set_String ("         Time_Slice_Value         => ");

         if Task_Dispatching_Policy_Specified = 'F'
           and then ALIs.Table (ALIs.First).Time_Slice_Value = -1
         then
            Set_Int (0);
         else
            Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
         end if;

         Set_Char   (',');
         Write_Statement_Buffer;

         Set_String ("         WC_Encoding              => '");
         Set_Char   (ALIs.Table (ALIs.First).WC_Encoding);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Locking_Policy           => '");
         Set_Char   (Locking_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Queuing_Policy           => '");
         Set_Char   (Queuing_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         Set_String ("         Task_Dispatching_Policy  => '");
         Set_Char   (Task_Dispatching_Policy_Specified);
         Set_String ("',");
         Write_Statement_Buffer;

         WBI ("         Adafinal                 => System.Null_Address,");

         Set_String ("         Unreserve_All_Interrupts => ");

         if Unreserve_All_Interrupts_Specified then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_String (",");
         Write_Statement_Buffer;

         Set_String ("         Exception_Tracebacks     => ");

         if Exception_Tracebacks then
            Set_String ("1");
         else
            Set_String ("0");
         end if;

         Set_String (");");
         Write_Statement_Buffer;

         --  Generate call to Install_Handler
         WBI ("");
         WBI ("      if Handler_Installed = 0 then");
         WBI ("        Install_Handler;");
         WBI ("      end if;");
      end if;

      Gen_Elab_Calls_Ada;

      WBI ("   end " & Ada_Init_Name.all & ";");
   end Gen_Adainit_Ada;

   -------------------
   -- Gen_Adainit_C --
   --------------------

   procedure Gen_Adainit_C is
      Main_Priority : Int renames ALIs.Table (ALIs.First).Main_Priority;
   begin
      WBI ("void " & Ada_Init_Name.all & " ()");
      WBI ("{");

      --  Generate externals for elaboration entities

      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
            U    : Unit_Record renames Units.Table (Unum);

         begin
            if U.Set_Elab_Entity then
               Set_String ("   extern char ");
               Get_Name_String (U.Uname);
               Set_Unit_Name;
               Set_String ("_E;");
               Write_Statement_Buffer;
            end if;
         end;
      end loop;

      Write_Statement_Buffer;

      if No_Run_Time_Specified then

         --  Case of No_Run_Time mode. Set __gl_main_priority if needed
         --  for the Ravenscar profile.

         if Main_Priority /= No_Main_Priority then
            Set_String ("   extern int __gl_main_priority = ");
            Set_Int    (Main_Priority);
            Set_Char   (';');
            Write_Statement_Buffer;
         end if;

      else
         --  Code for normal case (not in No_Run_Time mode)

         Gen_Exception_Table_C;

         --  Generate call to set the runtime global variables defined in
         --  a-init.c. We define the varables in a-init.c, rather than in
         --  the binder generated file itself to avoid undefined externals
         --  when the runtime is linked as a shareable image library.

         --  We call the routine from inside adainit() because this works for
         --  both programs with and without binder generated "main" functions.

         WBI ("   __gnat_set_globals (");

         Set_String ("      ");
         Set_Int (Main_Priority);
         Set_Char (',');
         Tab_To (15);
         Set_String ("/* Main_Priority              */");
         Write_Statement_Buffer;

         Set_String ("      ");

         if Task_Dispatching_Policy = 'F'
           and then ALIs.Table (ALIs.First).Time_Slice_Value = -1
         then
            Set_Int (0);
         else
            Set_Int (ALIs.Table (ALIs.First).Time_Slice_Value);
         end if;

         Set_Char   (',');
         Tab_To (15);
         Set_String ("/* Time_Slice_Value           */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char   (ALIs.Table (ALIs.First).WC_Encoding);
         Set_String ("',");
         Tab_To (15);
         Set_String ("/* WC_Encoding                */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Locking_Policy_Specified);
         Set_String ("',");
         Tab_To (15);
         Set_String ("/* Locking_Policy             */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Queuing_Policy_Specified);
         Set_String ("',");
         Tab_To (15);
         Set_String ("/* Queuing_Policy             */");
         Write_Statement_Buffer;

         Set_String ("      '");
         Set_Char (Task_Dispatching_Policy_Specified);
         Set_String ("',");
         Tab_To (15);
         Set_String ("/* Tasking_Dispatching_Policy */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_String ("0,");
         Tab_To (15);
         Set_String ("/* Finalization routine address, not used anymore */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_Int    (Boolean'Pos (Unreserve_All_Interrupts_Specified));
         Set_String (",");
         Tab_To (15);
         Set_String ("/* Unreserve_All_Interrupts */");
         Write_Statement_Buffer;

         Set_String ("      ");
         Set_Int    (Boolean'Pos (Exception_Tracebacks));
         Set_String (");");
         Tab_To (15);
         Set_String ("/* Exception_Tracebacks */");
         Write_Statement_Buffer;

         --  Install elaboration time signal handler
         WBI ("   if (__gnat_handler_installed == 0)");
         WBI ("     {");
         WBI ("        __gnat_install_handler ();");
         WBI ("     }");
      end if;

      WBI ("");
      Gen_Elab_Calls_C;
      WBI ("}");
   end Gen_Adainit_C;

   ------------------------
   -- Gen_Elab_Calls_Ada --
   ------------------------

   procedure Gen_Elab_Calls_Ada is
   begin

      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
            U    : Unit_Record renames Units.Table (Unum);

            Unum_Spec : Unit_Id;
            --  This is the unit number of the spec that corresponds to
            --  this entry. It is the same as Unum except when the body
            --  and spec are different and we are currently processing
            --  the body, in which case it is the spec (Unum + 1).

            procedure Set_Elab_Entity;
            --  Set name of elaboration entity flag

            procedure Set_Elab_Entity is
            begin
               Get_Decoded_Name_String_With_Brackets (U.Uname);
               Name_Len := Name_Len - 2;
               Set_Casing (U.Icasing);
               Set_Name_Buffer;
            end Set_Elab_Entity;

         begin
            if U.Utype = Is_Body then
               Unum_Spec := Unum + 1;
            else
               Unum_Spec := Unum;
            end if;

            --  Case of no elaboration code

            if U.No_Elab then

               --  The only case in which we have to do something is if
               --  this is a body, with a separate spec, where the separate
               --  spec has an elaboration entity defined.

               --  In that case, this is where we set the elaboration entity
               --  to True, we do not need to test if this has already been
               --  done, since it is quicker to set the flag than to test it.

               if U.Utype = Is_Body
                 and then Units.Table (Unum_Spec).Set_Elab_Entity
               then
                  Set_String ("      E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" := True;");
                  Write_Statement_Buffer;
               end if;

            --  Here if elaboration code is present. We generate:

            --    if not uname_E then
            --       uname'elab_[spec|body];
            --       uname_E := True;
            --    end if;

            --  The uname_E assignment is skipped if this is a separate spec,
            --  since the assignment will be done when we process the body.

            else
               Set_String ("      if not E");
               Set_Unit_Number (Unum_Spec);
               Set_String (" then");
               Write_Statement_Buffer;

               Set_String ("         ");
               Get_Decoded_Name_String_With_Brackets (U.Uname);

               if Name_Buffer (Name_Len) = 's' then
                  Name_Buffer (Name_Len - 1 .. Name_Len + 8) := "'elab_spec";
               else
                  Name_Buffer (Name_Len - 1 .. Name_Len + 8) := "'elab_body";
               end if;

               Name_Len := Name_Len + 8;
               Set_Casing (U.Icasing);
               Set_Name_Buffer;
               Set_Char (';');
               Write_Statement_Buffer;

               if U.Utype /= Is_Spec then
                  Set_String ("         E");
                  Set_Unit_Number (Unum_Spec);
                  Set_String (" := True;");
                  Write_Statement_Buffer;
               end if;

               WBI ("      end if;");
            end if;
         end;
      end loop;

   end Gen_Elab_Calls_Ada;

   ----------------------
   -- Gen_Elab_Calls_C --
   ----------------------

   procedure Gen_Elab_Calls_C is
   begin

      for E in Elab_Order.First .. Elab_Order.Last loop
         declare
            Unum : constant Unit_Id := Elab_Order.Table (E);
            U    : Unit_Record renames Units.Table (Unum);

            Unum_Spec : Unit_Id;
            --  This is the unit number of the spec that corresponds to
            --  this entry. It is the same as Unum except when the body
            --  and spec are different and we are currently processing
            --  the body, in which case it is the spec (Unum + 1).

         begin
            if U.Utype = Is_Body then
               Unum_Spec := Unum + 1;
            else
               Unum_Spec := Unum;
            end if;

            --  Case of no elaboration code

            if U.No_Elab then

               --  The only case in which we have to do something is if
               --  this is a body, with a separate spec, where the separate
               --  spec has an elaboration entity defined.

               --  In that case, this is where we set the elaboration entity
               --  to True, we do not need to test if this has already been
               --  done, since it is quicker to set the flag than to test it.

               if U.Utype = Is_Body
                 and then Units.Table (Unum_Spec).Set_Elab_Entity
               then
                  Set_String ("   ");
                  Get_Name_String (U.Uname);
                  Set_Unit_Name;
                  Set_String ("_E = 1;");
                  Write_Statement_Buffer;
               end if;

            --  Here if elaboration code is present. We generate:

            --    if (uname_E == 0) {
            --       uname__elab[s|b] ();
            --       uname_E++;
            --    }

            --  The uname_E assignment is skipped if this is a separate spec,
            --  since the assignment will be done when we process the body.

            else
               Set_String ("   if (");
               Get_Name_String (U.Uname);
               Set_Unit_Name;
               Set_String ("_E == 0) {");
               Write_Statement_Buffer;

               Set_String ("      ");
               Set_Unit_Name;
               Set_String ("___elab");
               Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
               Set_String (" ();");
               Write_Statement_Buffer;

               if U.Utype /= Is_Spec then
                  Set_String ("      ");
                  Set_Unit_Name;
                  Set_String ("_E++;");
                  Write_Statement_Buffer;
               end if;

               WBI ("   }");
            end if;
         end;
      end loop;

   end Gen_Elab_Calls_C;

   ----------------------
   -- Gen_Elab_Defs_C --
   ----------------------

   procedure Gen_Elab_Defs_C is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop

         --  Generate declaration of elaboration procedure if elaboration
         --  needed. Note that passive units are always excluded.

         if not Units.Table (Elab_Order.Table (E)).No_Elab then
            Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);
            Set_String ("extern void ");
            Set_Unit_Name;
            Set_String ("___elab");
            Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
            Set_String (" PARAMS ((void));");
            Write_Statement_Buffer;
         end if;

      end loop;

      WBI ("");
   end Gen_Elab_Defs_C;

   ------------------------
   -- Gen_Elab_Order_Ada --
   ------------------------

   procedure Gen_Elab_Order_Ada is
   begin
      WBI ("");
      WBI ("   -- BEGIN ELABORATION ORDER");

      for J in Elab_Order.First .. Elab_Order.Last loop
         Set_String ("   -- ");
         Get_Unit_Name_String (Units.Table (Elab_Order.Table (J)).Uname);
         Set_Name_Buffer;
         Write_Statement_Buffer;
      end loop;

      WBI ("   -- END ELABORATION ORDER");
   end Gen_Elab_Order_Ada;

   ----------------------
   -- Gen_Elab_Order_C --
   ----------------------

   procedure Gen_Elab_Order_C is
   begin
      WBI ("");
      WBI ("/* BEGIN ELABORATION ORDER");

      for J in Elab_Order.First .. Elab_Order.Last loop
         Get_Unit_Name_String (Units.Table (Elab_Order.Table (J)).Uname);
         Set_Name_Buffer;
         Write_Statement_Buffer;
      end loop;

      WBI ("   END ELABORATION ORDER */");
   end Gen_Elab_Order_C;

   -----------------------------
   -- Gen_Exception_Table_Ada --
   -----------------------------

   procedure Gen_Exception_Table_Ada is
      Num  : Nat;
      Last : ALI_Id := No_ALI_Id;

   begin
      if not Zero_Cost_Exceptions_Specified then
         WBI ("   begin");
         return;
      end if;

      --  The code we generate looks like

      --        procedure SDP_Table_Build
      --          (SDP_Addresses   : System.Address;
      --           SDP_Count       : Natural;
      --           Elab_Addresses  : System.Address;
      --           Elab_Addr_Count : Natural);
      --        pragma Import (C, SDP_Table_Build, "__gnat_SDP_Table_Build");
      --
      --        ST : aliased constant array (1 .. nnn) of System.Address := (
      --               unit_name_1'UET_Address,
      --               unit_name_2'UET_Address,
      --               ...
      --               unit_name_3'UET_Address,
      --
      --        EA : aliased constant array (1 .. eee) of System.Address := (
      --               adainit'Code_Address,
      --               adafinal'Code_Address,
      --               unit_name'elab[spec|body]'Code_Address,
      --               unit_name'elab[spec|body]'Code_Address,
      --               unit_name'elab[spec|body]'Code_Address,
      --               unit_name'elab[spec|body]'Code_Address);
      --
      --     begin
      --        SDP_Table_Build (ST'Address, nnn, EA'Address, eee);

      Num := 0;
      for A in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A).Unit_Exception_Table then
            Num := Num + 1;
            Last := A;
         end if;
      end loop;

      if Num = 0 then

         --  Happens with "gnatmake -a -f -gnatL ..."

         WBI (" ");
         WBI ("   begin");
         return;
      end if;

      WBI ("      procedure SDP_Table_Build");
      WBI ("        (SDP_Addresses   : System.Address;");
      WBI ("         SDP_Count       : Natural;");
      WBI ("         Elab_Addresses  : System.Address;");
      WBI ("         Elab_Addr_Count : Natural);");
      WBI ("      " &
           "pragma Import (C, SDP_Table_Build, ""__gnat_SDP_Table_Build"");");

      WBI (" ");
      Set_String ("      ST : aliased constant array (1 .. ");
      Set_Int (Num);
      Set_String (") of System.Address := (");

      if Num = 1 then
         Set_String ("1 => A1);");
         Write_Statement_Buffer;

      else
         Write_Statement_Buffer;

         for A in ALIs.First .. ALIs.Last loop
            if ALIs.Table (A).Unit_Exception_Table then
               Get_Decoded_Name_String_With_Brackets
                 (Units.Table (ALIs.Table (A).First_Unit).Uname);
               Set_Casing (Mixed_Case);
               Set_String ("        ");
               Set_String (Name_Buffer (1 .. Name_Len - 2));
               Set_String ("'UET_Address");

               if A = Last then
                  Set_String (");");
               else
                  Set_Char (',');
               end if;

               Write_Statement_Buffer;
            end if;
         end loop;
      end if;

      WBI (" ");
      Set_String ("      EA : aliased constant array (1 .. ");
      Set_Int (Num_Elab_Calls + 2);
      Set_String (") of System.Address := (");
      Write_Statement_Buffer;
      WBI ("        " & Ada_Init_Name.all & "'Code_Address,");

      --  If compiling for the JVM, we directly reference Adafinal because
      --  we don't import it via Do_Finalize (see Gen_Output_File_Ada).

      if Hostparm.Java_VM then
         Set_String ("        System.Standard_Library.Adafinal'Code_Address");
      else
         Set_String ("        Do_Finalize'Code_Address");
      end if;

      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Decoded_Name_String_With_Brackets
           (Units.Table (Elab_Order.Table (E)).Uname);

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;

         else
            Set_Char (',');
            Write_Statement_Buffer;
            Set_String ("        ");

            if Name_Buffer (Name_Len) = 's' then
               Name_Buffer (Name_Len - 1 .. Name_Len + 21) :=
                                        "'elab_spec'code_address";
            else
               Name_Buffer (Name_Len - 1 .. Name_Len + 21) :=
                                        "'elab_body'code_address";
            end if;

            Name_Len := Name_Len + 21;
            Set_Casing (Units.Table (Elab_Order.Table (E)).Icasing);
            Set_Name_Buffer;
         end if;
      end loop;

      Set_String (");");
      Write_Statement_Buffer;

      WBI (" ");
      WBI ("   begin");

      Set_String ("      SDP_Table_Build (ST'Address, ");
      Set_Int (Num);
      Set_String (", EA'Address, ");
      Set_Int (Num_Elab_Calls + 2);
      Set_String (");");
      Write_Statement_Buffer;
   end Gen_Exception_Table_Ada;

   ---------------------------
   -- Gen_Exception_Table_C --
   ---------------------------

   procedure Gen_Exception_Table_C is
      Num  : Nat;
      Num2 : Nat;

   begin
      if not Zero_Cost_Exceptions_Specified then
         return;
      end if;

      --  The code we generate looks like

      --     extern void *__gnat_unitname1__SDP;
      --     extern void *__gnat_unitname2__SDP;
      --     ...
      --
      --     void **st[nnn] = {
      --       &__gnat_unitname1__SDP,
      --       &__gnat_unitname2__SDP,
      --       ...
      --       &__gnat_unitnamen__SDP};
      --
      --     extern void unitname1__elabb ();
      --     extern void unitname2__elabb ();
      --     ...
      --
      --     void (*ea[eee]) () = {
      --       adainit,
      --       adafinal,
      --       unitname1___elab[b,s],
      --       unitname2___elab[b,s],
      --       ...
      --       unitnamen___elab[b,s]};
      --
      --     __gnat_SDP_Table_Build (&st, nnn, &ea, eee);

      Num := 0;
      for A in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A).Unit_Exception_Table then
            Num := Num + 1;

            Set_String ("   extern void *__gnat_");
            Get_Name_String (Units.Table (ALIs.Table (A).First_Unit).Uname);
            Set_Unit_Name;
            Set_String ("__SDP");
            Set_Char (';');
            Write_Statement_Buffer;
         end if;
      end loop;

      if Num = 0 then

         --  Happens with "gnatmake -a -f -gnatL ..."

         return;
      end if;

      WBI (" ");

      Set_String ("   void **st[");
      Set_Int (Num);
      Set_String ("] = {");
      Write_Statement_Buffer;

      Num2 := 0;
      for A in ALIs.First .. ALIs.Last loop
         if ALIs.Table (A).Unit_Exception_Table then
            Num2 := Num2 + 1;

            Set_String ("     &__gnat_");
            Get_Name_String (Units.Table (ALIs.Table (A).First_Unit).Uname);
            Set_Unit_Name;
            Set_String ("__SDP");

            if Num = Num2 then
               Set_String ("};");
            else
               Set_Char (',');
            end if;

            Write_Statement_Buffer;
         end if;
      end loop;

      WBI ("");
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;

         else
            Set_String ("   extern void ");
            Set_Unit_Name;
            Set_String ("___elab");
            Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
            Set_String (" ();");
            Write_Statement_Buffer;
         end if;
      end loop;

      WBI ("");
      Set_String ("   void (*ea[");
      Set_Int (Num_Elab_Calls + 2);
      Set_String ("]) () = {");
      Write_Statement_Buffer;

      WBI ("     " & Ada_Init_Name.all & ",");
      Set_String ("     system__standard_library__adafinal");

      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;

         else
            Set_Char (',');
            Write_Statement_Buffer;
            Set_String ("     ");
            Set_Unit_Name;
            Set_String ("___elab");
            Set_Char (Name_Buffer (Name_Len)); -- 's' or 'b' for spec/body
         end if;
      end loop;

      Set_String ("};");
      Write_Statement_Buffer;

      WBI (" ");

      Set_String ("   __gnat_SDP_Table_Build (&st, ");
      Set_Int (Num);
      Set_String (", ea, ");
      Set_Int (Num_Elab_Calls + 2);
      Set_String (");");
      Write_Statement_Buffer;
   end Gen_Exception_Table_C;

   ------------------
   -- Gen_Main_Ada --
   ------------------

   procedure Gen_Main_Ada is
      Target         : constant String_Ptr := Target_Name;
      VxWorks_Target : constant Boolean :=
                         Target (Target'Last - 7 .. Target'Last) = "vxworks/";

   begin
      WBI ("");
      Set_String ("   function ");
      Set_String (Get_Main_Name);

      if VxWorks_Target then
         Set_String (" return Integer is");
         Write_Statement_Buffer;

      else
         Write_Statement_Buffer;
         WBI ("     (argc : Integer;");
         WBI ("      argv : System.Address;");
         WBI ("      envp : System.Address)");
         WBI ("      return Integer");
         WBI ("   is");
      end if;

      --  Initialize and Finalize are not used in No_Run_Time mode

      if not No_Run_Time_Specified then
         WBI ("      procedure initialize;");
         WBI ("      pragma Import (C, initialize, ""__gnat_initialize"");");
         WBI ("");
         WBI ("      procedure finalize;");
         WBI ("      pragma Import (C, finalize, ""__gnat_finalize"");");
         WBI ("");
      end if;

      --  Deal with declarations for main program case

      if not No_Main_Subprogram then

         --  To call the main program, we declare it using a pragma Import
         --  Ada with the right link name.

         --  It might seem more obvious to "with" the main program, and call
         --  it in the normal Ada manner. We do not do this for three reasons:

         --    1. It is more efficient not to recompile the main program
         --    2. We are not entitled to assume the source is accessible
         --    3. We don't know what options to use to compile it

         --  It is really reason 3 that is most critical (indeed we used
         --  to generate the "with", but several regression tests failed).

         WBI ("");

         if ALIs.Table (ALIs.First).Main_Program = Func then
            WBI ("      Result : Integer;");
            WBI ("");
            WBI ("      function Ada_Main_Program return Integer;");

         else
            WBI ("      procedure Ada_Main_Program;");
         end if;

         Set_String ("      pragma Import (Ada, Ada_Main_Program, """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""");");

         Write_Statement_Buffer;
         WBI ("");
      end if;

      WBI ("   begin");

      --  On VxWorks, there are no command line arguments

      if VxWorks_Target then
         WBI ("      gnat_argc := 0;");
         WBI ("      gnat_argv := System.Null_Address;");
         WBI ("      gnat_envp := System.Null_Address;");

      --  Normal case of command line arguments present

      else
         WBI ("      gnat_argc := argc;");
         WBI ("      gnat_argv := argv;");
         WBI ("      gnat_envp := envp;");
         WBI ("");
      end if;

      if not No_Run_Time_Specified then
         WBI ("      Initialize;");
      end if;

      WBI ("      " & Ada_Init_Name.all & ";");

      if not No_Main_Subprogram then
         WBI ("      Break_Start;");

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            WBI ("      Ada_Main_Program;");
         else
            WBI ("      Result := Ada_Main_Program;");
         end if;
      end if;

      --  Adafinal is only called if we have a run time

      if not No_Run_Time_Specified then

         --  If compiling for the JVM, we directly call Adafinal because
         --  we don't import it via Do_Finalize (see Gen_Output_File_Ada).

         if Hostparm.Java_VM then
            WBI ("      System.Standard_Library.Adafinal;");
         else
            WBI ("      Do_Finalize;");
         end if;
      end if;

      --  Finalize is only called if we have a run time

      if not No_Run_Time_Specified then
         WBI ("      Finalize;");
      end if;

      --  Return result

      if No_Main_Subprogram
        or else ALIs.Table (ALIs.First).Main_Program = Proc
      then
         WBI ("      return (gnat_exit_status);");
      else
         WBI ("      return (Result);");
      end if;

      WBI ("   end;");
   end Gen_Main_Ada;

   ----------------
   -- Gen_Main_C --
   ----------------

   procedure Gen_Main_C is
      Target         : constant String_Ptr := Target_Name;
      VxWorks_Target : constant Boolean :=
                         Target (Target'Last - 7 .. Target'Last) = "vxworks/";

   begin
      Set_String ("int ");
      Set_String (Get_Main_Name);

      --  On VxWorks, there are no command line arguments

      if VxWorks_Target then
         Set_String (" ()");

      --  Normal case with command line arguments present

      else
         Set_String (" (argc, argv, envp)");
      end if;

      Write_Statement_Buffer;

      --  VxWorks doesn't have the notion of argc/argv

      if VxWorks_Target then
         WBI ("{");
         WBI ("   int result;");
         WBI ("   gnat_argc = 0;");
         WBI ("   gnat_argv = 0;");
         WBI ("   gnat_envp = 0;");

      --  Normal case of arguments present

      else
         WBI ("    int argc;");
         WBI ("    char **argv;");
         WBI ("    char **envp;");
         WBI ("{");

         if ALIs.Table (ALIs.First).Main_Program = Func then
            WBI ("   int result;");
         end if;

         WBI ("   gnat_argc = argc;");
         WBI ("   gnat_argv = argv;");
         WBI ("   gnat_envp = envp;");
         WBI (" ");
      end if;

      --  The __gnat_initialize routine is used only if we have a run-time

      if not No_Run_Time_Specified then
         WBI
          ("   __gnat_initialize ();");
      end if;

      WBI ("   " & Ada_Init_Name.all & " ();");

      if not No_Main_Subprogram then

         WBI ("   __gnat_break_start ();");
         WBI (" ");

         --  Output main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  Main program is procedure case

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            Set_String ("   ");
            Set_Main_Program_Name;
            Set_String (" ();");
            Write_Statement_Buffer;

         --  Main program is function case

         else -- ALIs.Table (ALIs_First).Main_Program = Func
            Set_String ("   result = ");
            Set_Main_Program_Name;
            Set_String (" ();");
            Write_Statement_Buffer;
         end if;

      end if;

      --  Adafinal is called only when we have a run-time

      if not No_Run_Time_Specified then
         WBI (" ");
         WBI ("   system__standard_library__adafinal ();");
      end if;

      --  The finalize routine is used only if we have a run-time

      if not No_Run_Time_Specified then
         WBI ("   __gnat_finalize ();");
      end if;

      if ALIs.Table (ALIs.First).Main_Program = Func then

         if Hostparm.OpenVMS then

            --  VMS must use the Posix exit routine in order to get an
            --  Unix compatible exit status.

            WBI ("   __posix_exit (result);");

         else
            WBI ("   exit (result);");
         end if;

      else

         if Hostparm.OpenVMS then
            --  VMS must use the Posix exit routine in order to get an
            --  Unix compatible exit status.
            WBI ("   __posix_exit (gnat_exit_status);");
         else
            WBI ("   exit (gnat_exit_status);");
         end if;
      end if;

      WBI ("}");
   end Gen_Main_C;

   ------------------------------
   -- Gen_Object_Files_Options --
   ------------------------------

   procedure Gen_Object_Files_Options is
      Lgnat                     : Integer;

      procedure Write_Linker_Option;
      --  Write binder info linker option.

      -------------------------
      -- Write_Linker_Option --
      -------------------------

      procedure Write_Linker_Option is
         Start : Natural;
         Stop  : Natural;

      begin
         --  Loop through string, breaking at null's

         Start := 1;
         while Start < Name_Len loop

            --  Find null ending this section

            Stop := Start + 1;
            while Name_Buffer (Stop) /= ASCII.NUL
              and then Stop <= Name_Len loop
               Stop := Stop + 1;
            end loop;

            --  Process section if non-null

            if Stop > Start then
                  if Output_Linker_Option_List then
                     Write_Str (Name_Buffer (Start .. Stop - 1));
                     Write_Eol;
                  end if;
                  Write_Info_Ada_C
                    ("   --   ", "", Name_Buffer (Start .. Stop - 1));
            end if;

            Start := Stop + 1;
         end loop;
      end Write_Linker_Option;

   --  Start of processing for Gen_Object_Files_Options

   begin
      WBI ("");
      Write_Info_Ada_C ("--", "/*", " BEGIN Object file/option list");

      for E in Elab_Order.First .. Elab_Order.Last loop

         --  If not spec that has an associated body, then generate a
         --  comment giving the name of the corresponding object file.

         if Units.Table (Elab_Order.Table (E)).Utype /= Is_Spec then
            Get_Name_String
              (ALIs.Table
                (Units.Table (Elab_Order.Table (E)).My_ALI).Ofile_Full_Name);

            --  If the presence of an object file is necessary or if it
            --  exists, then use it.

            if not Hostparm.Exclude_Missing_Objects
              or else
                GNAT.OS_Lib.Is_Regular_File (Name_Buffer (1 .. Name_Len))
            then
               Write_Info_Ada_C ("   --   ", "", Name_Buffer (1 .. Name_Len));
               if Output_Object_List then
                  Write_Str (Name_Buffer (1 .. Name_Len));
                  Write_Eol;
               end if;

               --  Don't link with the shared library on VMS if an internal
               --  filename object is seen. Multiply defined symbols will
               --  result.

               if Hostparm.OpenVMS
                 and then Is_Internal_File_Name
                  (ALIs.Table
                   (Units.Table (Elab_Order.Table (E)).My_ALI).Sfile)
               then
                  Opt.Shared_Libgnat := False;
               end if;

            end if;
         end if;
      end loop;

      --  Add a "-Ldir" for each directory in the object path. We skip this
      --  in No_Run_Time mode, where we want more precise control of exactly
      --  what goes into the resulting object file

      if not No_Run_Time_Specified then
         for J in 1 .. Nb_Dir_In_Obj_Search_Path loop
            declare
               Dir : String_Ptr := Dir_In_Obj_Search_Path (J);

            begin
               Name_Len := 0;
               Add_Str_To_Name_Buffer ("-L");
               Add_Str_To_Name_Buffer (Dir.all);
               Write_Linker_Option;
            end;
         end loop;
      end if;

      --  Sort linker options

      Sort (Linker_Options.Last, Move_Linker_Option'Access,
                                    Lt_Linker_Option'Access);

      --  Write user linker options

      Lgnat := Linker_Options.Last + 1;

      for J in 1 .. Linker_Options.Last loop
         if not Linker_Options.Table (J).Internal_File then
            Get_Name_String (Linker_Options.Table (J).Name);
            Write_Linker_Option;
         else
            Lgnat := J;
            exit;
         end if;
      end loop;

      if not (No_Run_Time_Specified or else Opt.No_Stdlib) then

         Name_Len := 0;

         if Opt.Shared_Libgnat then
            Add_Str_To_Name_Buffer ("-shared");
         else
            Add_Str_To_Name_Buffer ("-static");
         end if;

         --  Write directly to avoid -K output.

         Write_Info_Ada_C ("   --   ", "", Name_Buffer (1 .. Name_Len));

         if With_DECGNAT then
            Name_Len := 0;
            Add_Str_To_Name_Buffer ("-ldecgnat");
            Write_Linker_Option;
         end if;

         if With_GNARL then
            Name_Len := 0;
            Add_Str_To_Name_Buffer ("-lgnarl");
            Write_Linker_Option;
         end if;

         Name_Len := 0;
         Add_Str_To_Name_Buffer ("-lgnat");
         Write_Linker_Option;

      end if;

      --  Write internal linker options

      for J in Lgnat .. Linker_Options.Last loop
         Get_Name_String (Linker_Options.Table (J).Name);
         Write_Linker_Option;
      end loop;

      if Ada_Bind_File then
         WBI ("-- END Object file/option list   ");
      else
         WBI ("   END Object file/option list */");
      end if;

   end Gen_Object_Files_Options;

   ---------------------
   -- Gen_Output_File --
   ---------------------

   procedure Gen_Output_File (Filename : String) is

   --  Start of processing for Gen_Output_File

   begin
      --  Override Ada_Bind_File and Bind_Main_Program for Java since
      --  JGNAT only supports Ada code, and the main program is already
      --  generated by the compiler.

      if Hostparm.Java_VM then
         Ada_Bind_File := True;
         Bind_Main_Program := False;
      end if;

      --  Override time slice value if -T switch is set

      if Time_Slice_Set then
         ALIs.Table (ALIs.First).Time_Slice_Value := Opt.Time_Slice_Value;
      end if;

      --  Count number of elaboration calls

      for E in Elab_Order.First .. Elab_Order.Last loop
         if Units.Table (Elab_Order.Table (E)).No_Elab then
            null;
         else
            Num_Elab_Calls := Num_Elab_Calls + 1;
         end if;
      end loop;

      --  Generate output file in appropriate language

      if Ada_Bind_File then
         Gen_Output_File_Ada (Filename);
      else
         Gen_Output_File_C (Filename);
      end if;

   end Gen_Output_File;

   -------------------------
   -- Gen_Output_File_Ada --
   -------------------------

   procedure Gen_Output_File_Ada (Filename : String) is

      Bfiles : Name_Id;
      --  Name of generated bind file (spec)

      Bfileb : Name_Id;
      --  Name of generated bind file (body)

      Ada_Main : constant String := Get_Ada_Main_Name;
      --  Name to be used for generated Ada main program. See the body of
      --  function Get_Ada_Main_Name for details on the form of the name.

      Target         : constant String_Ptr := Target_Name;
      VxWorks_Target : constant Boolean :=
                         Target (Target'Last - 7 .. Target'Last) = "vxworks/";

   begin
      --  Create spec first

      Create_Binder_Output (Filename, 's', Bfiles);

      if No_Run_Time_Specified then
         WBI ("pragma No_Run_Time;");
      end if;

      --  Generate with of System so we can reference System.Address, note
      --  that such a reference is safe even in No_Run_Time mode, since we
      --  do not need any run-time code for such a reference, and we output
      --  a pragma No_Run_Time for this compilation above.

      WBI ("with System;");

      --  Generate with of System.Initialize_Scalars if active

      if Initialize_Scalars_Used then
         WBI ("with System.Scalar_Values;");
      end if;

      Resolve_Binder_Options;

      if not No_Run_Time_Specified then

         --  Usually, adafinal is called using a pragma Import C. Since
         --  Import C doesn't have the same semantics for JGNAT, we use
         --  standard Ada.

         if Hostparm.Java_VM then
            WBI ("with System.Standard_Library;");
         end if;
      end if;

      WBI ("package " & Ada_Main & " is");

      --  Main program case

      if Bind_Main_Program then

         --  Generate argc/argv stuff

         WBI ("");
         WBI ("   gnat_argc : Integer;");
         WBI ("   gnat_argv : System.Address;");
         WBI ("   gnat_envp : System.Address;");

         --  If we have a run time present, these variables are in the
         --  runtime data area for easy access from the runtime

         if not No_Run_Time_Specified then
            WBI ("");
            WBI ("   pragma Import (C, gnat_argc);");
            WBI ("   pragma Import (C, gnat_argv);");
            WBI ("   pragma Import (C, gnat_envp);");
         end if;

         --  Define exit status. Again in normal mode, this is in the
         --  run-time library, and is initialized there, but in the no
         --  run time case, the variable is here and initialized here.

         WBI ("");

         if No_Run_Time_Specified then
            WBI ("   gnat_exit_status : Integer := 0;");
         else
            WBI ("   gnat_exit_status : Integer;");
            WBI ("   pragma Import (C, gnat_exit_status);");
         end if;
      end if;

      --  Generate the GNAT_Version and Ada_Main_Program_name info only for
      --  the main program. Otherwise, it can lead under some circumstances
      --  to a symbol duplication during the link (for instance when a
      --  C program uses 2 Ada libraries)

      if Bind_Main_Program then
         WBI ("");
         WBI ("   GNAT_Version : constant String :=");
         WBI ("                    ""GNAT Version: " &
                                   Gnat_Version_String & """;");
         WBI ("   pragma Export (C, GNAT_Version, ""__gnat_version"");");

         WBI ("");
         Set_String ("   Ada_Main_Program_Name : constant String := """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""" & Ascii.NUL;");
         Write_Statement_Buffer;

         WBI
           ("   pragma Export (C, Ada_Main_Program_Name, " &
            """__gnat_ada_main_program_name"");");
      end if;

      --  No need to generate a finalization routine if there is no
      --  runtime, since there is nothing to do in this case.

      if not No_Run_Time_Specified then
         WBI ("");
         WBI ("   procedure " & Ada_Final_Name.all & ";");
         WBI ("   pragma Export (C, " & Ada_Final_Name.all & ", """ &
              Ada_Final_Name.all & """);");
      end if;

      WBI ("");
      WBI ("   procedure " & Ada_Init_Name.all & ";");
      WBI ("   pragma Export (C, " & Ada_Init_Name.all & ", """ &
           Ada_Init_Name.all & """);");

      if Bind_Main_Program then

         --  If we have a run time, then Break_Start is defined there, but
         --  if there is no run-time, Break_Start is defined in this file.

         WBI ("");
         WBI ("   procedure Break_Start;");

         if No_Run_Time_Specified then
            WBI ("   pragma Export (C, Break_Start, ""__gnat_break_start"");");
         else
            WBI ("   pragma Import (C, Break_Start, ""__gnat_break_start"");");
         end if;

         WBI ("");
         WBI ("   function " & Get_Main_Name);

         --  Generate argument list (except on VxWorks, where none is present)

         if not VxWorks_Target then
            WBI ("     (argc : Integer;");
            WBI ("      argv : System.Address;");
            WBI ("      envp : System.Address)");
         end if;

         WBI ("      return Integer;");
         WBI ("   pragma Export (C, " & Get_Main_Name & ", """ &
           Get_Main_Name & """);");
      end if;

      if Initialize_Scalars_Used then
         Gen_Scalar_Values;
      end if;

      Gen_Versions_Ada;
      Gen_Elab_Order_Ada;

      --  Spec is complete

      WBI ("");
      WBI ("end " & Ada_Main & ";");
      Close_Binder_Output;

      --  Prepare to write body

      Create_Binder_Output (Filename, 'b', Bfileb);

      --  Output Source_File_Name pragmas which look like

      --    pragma Source_File_Name (Ada_Main, Spec_File_Name => "sss");
      --    pragma Source_File_Name (Ada_Main, Body_File_Name => "bbb");

      --  where sss/bbb are the spec/body file names respectively

      Get_Name_String (Bfiles);
      Name_Buffer (Name_Len + 1 .. Name_Len + 3) := """);";

      WBI ("pragma Source_File_Name (" &
           Ada_Main &
           ", Spec_File_Name => """ &
           Name_Buffer (1 .. Name_Len + 3));

      Get_Name_String (Bfileb);
      Name_Buffer (Name_Len + 1 .. Name_Len + 3) := """);";

      WBI ("pragma Source_File_Name (" &
           Ada_Main &
           ", Body_File_Name => """ &
           Name_Buffer (1 .. Name_Len + 3));

      WBI ("");
      WBI ("package body " & Ada_Main & " is");

      --  Import the finalization procedure only if there is a runtime.

      if not No_Run_Time_Specified then

         --  In the Java case, pragma Import C cannot be used, so the
         --  standard Ada constructs will be used instead.

         if not Hostparm.Java_VM then
            WBI ("");
            WBI ("   procedure Do_Finalize;");
            WBI
              ("   pragma Import (C, Do_Finalize, " &
               """system__standard_library__adafinal"");");
            WBI ("");
         end if;
      end if;

      Gen_Adainit_Ada;

      --  No need to generate a finalization routine if there is no
      --  runtime, since there is nothing to do in this case.

      if not No_Run_Time_Specified then
         Gen_Adafinal_Ada;
      end if;

      if Bind_Main_Program then

         --  In No_Run_Time mode, generate dummy body for Break_Start

         if No_Run_Time_Specified then
            WBI ("");
            WBI ("   procedure Break_Start is");
            WBI ("   begin");
            WBI ("      null;");
            WBI ("   end;");
         end if;

         Gen_Main_Ada;
      end if;

      --  Output object file list and the Ada body is complete

      Gen_Object_Files_Options;

      WBI ("");
      WBI ("end " & Ada_Main & ";");

      Close_Binder_Output;
   end Gen_Output_File_Ada;

   -----------------------
   -- Gen_Output_File_C --
   -----------------------

   procedure Gen_Output_File_C (Filename : String) is

      Bfile : Name_Id;
      --  Name of generated bind file

   begin
      Create_Binder_Output (Filename, 'c', Bfile);

      Resolve_Binder_Options;

      WBI ("#ifdef __STDC__");
      WBI ("#define PARAMS(paramlist) paramlist");
      WBI ("#else");
      WBI ("#define PARAMS(paramlist) ()");
      WBI ("#endif");
      WBI ("");

      WBI ("extern void __gnat_set_globals ");
      WBI (" PARAMS ((int, int, int, int, int, int, ");
      WBI ("          void (*) PARAMS ((void)), int, int));");
      WBI ("extern void " & Ada_Final_Name.all & " PARAMS ((void));");
      WBI ("extern void " & Ada_Init_Name.all & " PARAMS ((void));");

      WBI ("extern void system__standard_library__adafinal PARAMS ((void));");

      if not No_Main_Subprogram then
         WBI ("extern int main PARAMS ((int, char **, char **));");
         if Hostparm.OpenVMS then
            WBI ("extern void __posix_exit PARAMS ((int));");
         else
            WBI ("extern void exit PARAMS ((int));");
         end if;

         WBI ("extern void __gnat_break_start PARAMS ((void));");
         Set_String ("extern ");

         if ALIs.Table (ALIs.First).Main_Program = Proc then
            Set_String ("void ");
         else
            Set_String ("int ");
         end if;

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (" PARAMS ((void));");
         Write_Statement_Buffer;
      end if;

      if not No_Run_Time_Specified then
         WBI ("extern void __gnat_initialize PARAMS ((void));");
         WBI ("extern void __gnat_finalize PARAMS ((void));");
         WBI ("extern void __gnat_install_handler PARAMS ((void));");
      end if;

      WBI ("");

      Gen_Elab_Defs_C;

      --  Imported variable used to track elaboration/finalization phase.
      --  Used only when we have a runtime.

      if not No_Run_Time_Specified then
         WBI ("extern int  __gnat_handler_installed;");
         WBI ("");
      end if;

      --  Write argv/argc stuff if main program case

      if Bind_Main_Program then

         --  In the normal case, these are in the runtime library

         if not No_Run_Time_Specified then
            WBI ("extern int gnat_argc;");
            WBI ("extern char **gnat_argv;");
            WBI ("extern char **gnat_envp;");
            WBI ("extern int gnat_exit_status;");

         --  In the No_Run_Time case, they are right in the binder file
         --  and we initialize gnat_exit_status in the declaration.

         else
            WBI ("int gnat_argc;");
            WBI ("char **gnat_argv;");
            WBI ("char **gnat_envp;");
            WBI ("int gnat_exit_status = 0;");
         end if;

         WBI ("");
      end if;

      --  In no run-time mode, the __gnat_break_start routine (for the
      --  debugger to get initial control) is defined in this file.

      if No_Run_Time_Specified then
         WBI ("");
         WBI ("void __gnat_break_start () {}");
      end if;

      --  Generate the __gnat_version and __gnat_ada_main_program_name info
      --  only for the main program. Otherwise, it can lead under some
      --  circumstances to a symbol duplication during the link (for instance
      --  when a C program uses 2 Ada libraries)

      if Bind_Main_Program then
         WBI ("");
         WBI ("char __gnat_version[] = ""GNAT Version: " &
                                   Gnat_Version_String & """;");

         Set_String ("char __gnat_ada_main_program_name[] = """);
         Get_Name_String (Units.Table (First_Unit_Entry).Uname);
         Set_Main_Program_Name;
         Set_String (""";");
         Write_Statement_Buffer;
      end if;

      --  Generate the adafinal routine. In no runtime mode, this is
      --  not needed, since there is no finalization to do.

      if not No_Run_Time_Specified then
         Gen_Adafinal_C;
      end if;

      Gen_Adainit_C;

      --  Main is only present for Ada main case

      if Bind_Main_Program then
         Gen_Main_C;
      end if;

      --  Scalar values, versions and object files needed in both cases

      if Initialize_Scalars_Used then
         Gen_Scalar_Values;
      end if;

      Gen_Versions_C;
      Gen_Elab_Order_C;
      Gen_Object_Files_Options;

      --  C binder output is complete

      Close_Binder_Output;
   end Gen_Output_File_C;

   -----------------------
   -- Gen_Scalar_Values --
   -----------------------

   procedure Gen_Scalar_Values is

      --  Strings to hold hex values of initialization constants. Note that
      --  we store these strings in big endian order, but they are actually
      --  used to initialize integer values, so the actual generated data
      --  will automaticaly have the right endianess.

      IS_Is1 : String (1 .. 2);
      IS_Is2 : String (1 .. 4);
      IS_Is4 : String (1 .. 8);
      IS_Is8 : String (1 .. 16);
      IS_Iu1 : String (1 .. 2);
      IS_Iu2 : String (1 .. 4);
      IS_Iu4 : String (1 .. 8);
      IS_Iu8 : String (1 .. 16);
      IS_Isf : String (1 .. 8);
      IS_Ifl : String (1 .. 8);
      IS_Ilf : String (1 .. 16);

      --  The string for Long_Long_Float is special. This is used only on the
      --  ia32 with 80-bit extended float (stored in 96 bits by gcc). The
      --  value here is represented little-endian, since that's the only way
      --  it is ever generated (this is not used on big-endian machines.

      IS_Ill : String (1 .. 24);

   begin
      --  -Sin (invalid values)

      if Opt.Initialize_Scalars_Mode = 'I' then
         IS_Is1 := "80";
         IS_Is2 := "8000";
         IS_Is4 := "80000000";
         IS_Is8 := "8000000000000000";
         IS_Iu1 := "FF";
         IS_Iu2 := "FFFF";
         IS_Iu4 := "FFFFFFFF";
         IS_Iu8 := "FFFFFFFFFFFFFFFF";
         IS_Isf := IS_Iu4;
         IS_Ifl := IS_Iu4;
         IS_Ilf := IS_Iu8;
         IS_Ill := "00000000000000C0FFFF0000";

      --  -Slo (low values)

      elsif Opt.Initialize_Scalars_Mode = 'L' then
         IS_Is1 := "80";
         IS_Is2 := "8000";
         IS_Is4 := "80000000";
         IS_Is8 := "8000000000000000";
         IS_Iu1 := "00";
         IS_Iu2 := "0000";
         IS_Iu4 := "00000000";
         IS_Iu8 := "0000000000000000";
         IS_Isf := "FF800000";
         IS_Ifl := IS_Isf;
         IS_Ilf := "FFF0000000000000";
         IS_Ill := "0000000000000080FFFF0000";

      --  -Shi (high values)

      elsif Opt.Initialize_Scalars_Mode = 'H' then
         IS_Is1 := "7F";
         IS_Is2 := "7FFF";
         IS_Is4 := "7FFFFFFF";
         IS_Is8 := "7FFFFFFFFFFFFFFF";
         IS_Iu1 := "FF";
         IS_Iu2 := "FFFF";
         IS_Iu4 := "FFFFFFFF";
         IS_Iu8 := "FFFFFFFFFFFFFFFF";
         IS_Isf := "7F800000";
         IS_Ifl := IS_Isf;
         IS_Ilf := "7FF0000000000000";
         IS_Ill := "0000000000000080FF7F0000";

      --  -Shh (hex byte)

      else pragma Assert (Opt.Initialize_Scalars_Mode = 'X');
         IS_Is1 (1  .. 2)  := Opt.Initialize_Scalars_Val;
         IS_Is2 (1  .. 2)  := Opt.Initialize_Scalars_Val;
         IS_Is2 (3  .. 4)  := Opt.Initialize_Scalars_Val;

         for J in 1 .. 4 loop
            IS_Is4 (2 * J - 1 .. 2 * J) := Opt.Initialize_Scalars_Val;
         end loop;

         for J in 1 .. 8 loop
            IS_Is8 (2 * J - 1 .. 2 * J) := Opt.Initialize_Scalars_Val;
         end loop;

         IS_Iu1 := IS_Is1;
         IS_Iu2 := IS_Is2;
         IS_Iu4 := IS_Is4;
         IS_Iu8 := IS_Is8;

         IS_Isf := IS_Is4;
         IS_Ifl := IS_Is4;
         IS_Ilf := IS_Is8;

         for J in 1 .. 12 loop
            IS_Ill (2 * J - 1 .. 2 * J) := Opt.Initialize_Scalars_Val;
         end loop;
      end if;

      --  Generate output, Ada case

      if Ada_Bind_File then
         WBI ("");

         Set_String ("   IS_Is1 : constant System.Scalar_Values.Byte1 := 16#");
         Set_String (IS_Is1);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Is2 : constant System.Scalar_Values.Byte2 := 16#");
         Set_String (IS_Is2);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Is4 : constant System.Scalar_Values.Byte4 := 16#");
         Set_String (IS_Is4);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Is8 : constant System.Scalar_Values.Byte8 := 16#");
         Set_String (IS_Is8);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Iu1 : constant System.Scalar_Values.Byte1 := 16#");
         Set_String (IS_Iu1);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Iu2 : constant System.Scalar_Values.Byte2 := 16#");
         Set_String (IS_Iu2);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Iu4 : constant System.Scalar_Values.Byte4 := 16#");
         Set_String (IS_Iu4);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Iu8 : constant System.Scalar_Values.Byte8 := 16#");
         Set_String (IS_Iu8);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Isf : constant System.Scalar_Values.Byte4 := 16#");
         Set_String (IS_Isf);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Ifl : constant System.Scalar_Values.Byte4 := 16#");
         Set_String (IS_Ifl);
         Write_Statement_Buffer ("#;");

         Set_String ("   IS_Ilf : constant System.Scalar_Values.Byte8 := 16#");
         Set_String (IS_Ilf);
         Write_Statement_Buffer ("#;");

         --  Special case of Long_Long_Float. This is a 10-byte value used
         --  only on the x86. We could omit it for other architectures, but
         --  we don't easily have that kind of target specialization in the
         --  binder, and it's only 10 bytes, and only if -Sxx is used. Note
         --  that for architectures where Long_Long_Float is the same as
         --  Long_Float, the expander uses the Long_Float constant for the
         --  initializations of Long_Long_Float values.

         WBI ("   IS_Ill : constant array (1 .. 12) of");
         WBI ("              System.Scalar_Values.Byte1 := (");
         Set_String ("               ");

         for J in 1 .. 6 loop
            Set_String (" 16#");
            Set_Char (IS_Ill (2 * J - 1));
            Set_Char (IS_Ill (2 * J));
            Set_String ("#,");
         end loop;

         Write_Statement_Buffer;
         Set_String ("               ");

         for J in 7 .. 12 loop
            Set_String (" 16#");
            Set_Char (IS_Ill (2 * J - 1));
            Set_Char (IS_Ill (2 * J));

            if J = 12 then
               Set_String ("#);");
            else
               Set_String ("#,");
            end if;
         end loop;

         Write_Statement_Buffer;

         --  Output export statements to export to System.Scalar_Values

         WBI ("");

         WBI ("   pragma Export (Ada, IS_Is1, ""__gnat_Is1"");");
         WBI ("   pragma Export (Ada, IS_Is2, ""__gnat_Is2"");");
         WBI ("   pragma Export (Ada, IS_Is4, ""__gnat_Is4"");");
         WBI ("   pragma Export (Ada, IS_Is8, ""__gnat_Is8"");");
         WBI ("   pragma Export (Ada, IS_Iu1, ""__gnat_Iu1"");");
         WBI ("   pragma Export (Ada, IS_Iu2, ""__gnat_Iu2"");");
         WBI ("   pragma Export (Ada, IS_Iu4, ""__gnat_Iu4"");");
         WBI ("   pragma Export (Ada, IS_Iu8, ""__gnat_Iu8"");");
         WBI ("   pragma Export (Ada, IS_Isf, ""__gnat_Isf"");");
         WBI ("   pragma Export (Ada, IS_Ifl, ""__gnat_Ifl"");");
         WBI ("   pragma Export (Ada, IS_Ilf, ""__gnat_Ilf"");");
         WBI ("   pragma Export (Ada, IS_Ill, ""__gnat_Ill"");");

      --  Generate output C case

      else
         --  The lines we generate in this case are of the form
         --    typ __gnat_I?? = 0x??;
         --  where typ is appropriate to the length

         WBI ("");

         Set_String ("unsigned char      __gnat_Is1 = 0x");
         Set_String (IS_Is1);
         Write_Statement_Buffer (";");

         Set_String ("unsigned short     __gnat_Is2 = 0x");
         Set_String (IS_Is2);
         Write_Statement_Buffer (";");

         Set_String ("unsigned           __gnat_Is4 = 0x");
         Set_String (IS_Is4);
         Write_Statement_Buffer (";");

         Set_String ("long long unsigned __gnat_Is8 = 0x");
         Set_String (IS_Is8);
         Write_Statement_Buffer ("LL;");

         Set_String ("unsigned char      __gnat_Iu1 = 0x");
         Set_String (IS_Is1);
         Write_Statement_Buffer (";");

         Set_String ("unsigned short     __gnat_Iu2 = 0x");
         Set_String (IS_Is2);
         Write_Statement_Buffer (";");

         Set_String ("unsigned           __gnat_Iu4 = 0x");
         Set_String (IS_Is4);
         Write_Statement_Buffer (";");

         Set_String ("long long unsigned __gnat_Iu8 = 0x");
         Set_String (IS_Is8);
         Write_Statement_Buffer ("LL;");

         Set_String ("unsigned           __gnat_Isf = 0x");
         Set_String (IS_Isf);
         Write_Statement_Buffer (";");

         Set_String ("unsigned           __gnat_Ifl = 0x");
         Set_String (IS_Ifl);
         Write_Statement_Buffer (";");

         Set_String ("long long unsigned __gnat_Ilf = 0x");
         Set_String (IS_Ilf);
         Write_Statement_Buffer ("LL;");

         --  For Long_Long_Float, we generate
         --    char __gnat_Ill[12] = {0x??, 0x??, 0x??, 0x??, 0x??, 0x??,
         --                           0x??, 0x??, 0x??, 0x??, 0x??, 0x??);

         Set_String ("unsigned char      __gnat_Ill[12] = {");

         for J in 1 .. 6 loop
            Set_String ("0x");
            Set_Char (IS_Ill (2 * J - 1));
            Set_Char (IS_Ill (2 * J));
            Set_String (", ");
         end loop;

         Write_Statement_Buffer;
         Set_String ("                                     ");

         for J in 7 .. 12 loop
            Set_String ("0x");
            Set_Char (IS_Ill (2 * J - 1));
            Set_Char (IS_Ill (2 * J));

            if J = 12 then
               Set_String ("};");
            else
               Set_String (", ");
            end if;
         end loop;

         Write_Statement_Buffer;
      end if;
   end Gen_Scalar_Values;

   ----------------------
   -- Gen_Versions_Ada --
   ----------------------

   --  This routine generates two sets of lines. The first set has the form:

   --    unnnnn : constant Integer := 16#hhhhhhhh#;

   --  The second set has the form

   --    pragma Export (C, unnnnn, unam);

   --  for each unit, where unam is the unit name suffixed by either B or
   --  S for body or spec, with dots replaced by double underscores, and
   --  hhhhhhhh is the version number, and nnnnn is a 5-digits serial number.

   procedure Gen_Versions_Ada is
      Ubuf : String (1 .. 6) := "u00000";

      procedure Increment_Ubuf;
      --  Little procedure to increment the serial number

      procedure Increment_Ubuf is
      begin
         for J in reverse Ubuf'Range loop
            Ubuf (J) := Character'Succ (Ubuf (J));
            exit when Ubuf (J) <= '9';
            Ubuf (J) := '0';
         end loop;
      end Increment_Ubuf;

   --  Start of processing for Gen_Versions_Ada

   begin
      if Bind_For_Library then

         --  When building libraries, the version number of each unit can
         --  not be computed, since the binder does not know the full list
         --  of units. Therefore, the 'Version and 'Body_Version
         --  attributes can not supported in this case.

         return;
      end if;

      WBI ("");

      WBI ("   type Version_32 is mod 2 ** 32;");
      for U in Units.First .. Units.Last loop
         Increment_Ubuf;
         WBI ("   " & Ubuf & " : constant Version_32 := 16#" &
              Units.Table (U).Version & "#;");
      end loop;

      WBI ("");
      Ubuf := "u00000";

      for U in Units.First .. Units.Last loop
         Increment_Ubuf;
         Set_String ("   pragma Export (C, ");
         Set_String (Ubuf);
         Set_String (", """);

         Get_Name_String (Units.Table (U).Uname);

         for K in 1 .. Name_Len loop
            if Name_Buffer (K) = '.' then
               Set_Char ('_');
               Set_Char ('_');

            elsif Name_Buffer (K) = '%' then
               exit;

            else
               Set_Char (Name_Buffer (K));
            end if;
         end loop;

         if Name_Buffer (Name_Len) = 's' then
            Set_Char ('S');
         else
            Set_Char ('B');
         end if;

         Set_String (""");");
         Write_Statement_Buffer;
      end loop;

   end Gen_Versions_Ada;

   --------------------
   -- Gen_Versions_C --
   --------------------

   --  This routine generates a line of the form:

   --    unsigned unam = 0xhhhhhhhh;

   --  for each unit, where unam is the unit name suffixed by either B or
   --  S for body or spec, with dots replaced by double underscores.

   procedure Gen_Versions_C is
   begin
      if Bind_For_Library then

         --  When building libraries, the version number of each unit can
         --  not be computed, since the binder does not know the full list
         --  of units. Therefore, the 'Version and 'Body_Version
         --  attributes can not supported.

         return;
      end if;

      for U in Units.First .. Units.Last loop
         Set_String ("unsigned ");

         Get_Name_String (Units.Table (U).Uname);

         for K in 1 .. Name_Len loop
            if Name_Buffer (K) = '.' then
               Set_String ("__");

            elsif Name_Buffer (K) = '%' then
               exit;

            else
               Set_Char (Name_Buffer (K));
            end if;
         end loop;

         if Name_Buffer (Name_Len) = 's' then
            Set_Char ('S');
         else
            Set_Char ('B');
         end if;

         Set_String (" = 0x");
         Set_String (Units.Table (U).Version);
         Set_Char   (';');
         Write_Statement_Buffer;
      end loop;

   end Gen_Versions_C;

   -----------------------
   -- Get_Ada_Main_Name --
   -----------------------

   function Get_Ada_Main_Name return String is
      Suffix : constant String := "_00";
      Name   : String (1 .. Opt.Ada_Main_Name.all'Length + Suffix'Length) :=
                 Opt.Ada_Main_Name.all & Suffix;
      Nlen   : Natural;

   begin
      --  The main program generated by JGNAT expects a package called
      --  ada_<main procedure>.

      if Hostparm.Java_VM then
         --  Get main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  Remove the %b

         return "ada_" & Name_Buffer (1 .. Name_Len - 2);
      end if;

      --  This loop tries the following possibilities in order
      --    <Ada_Main>
      --    <Ada_Main>_01
      --    <Ada_Main>_02
      --    ..
      --    <Ada_Main>_99
      --  where <Ada_Main> is equal to Opt.Ada_Main_Name. By default,
      --  it is set to 'ada_main'.

      for J in 0 .. 99 loop
         if J = 0 then
            Nlen := Name'Length - Suffix'Length;
         else
            Nlen := Name'Length;
            Name (Name'Last) := Character'Val (J mod 10 + Character'Pos ('0'));
            Name (Name'Last - 1) :=
              Character'Val (J /   10 + Character'Pos ('0'));
         end if;

         for K in ALIs.First .. ALIs.Last loop
            for L in ALIs.Table (K).First_Unit .. ALIs.Table (K).Last_Unit loop

               --  Get unit name, removing %b or %e at end

               Get_Name_String (Units.Table (L).Uname);
               Name_Len := Name_Len - 2;

               if Name_Buffer (1 .. Name_Len) = Name (1 .. Nlen) then
                  goto Continue;
               end if;
            end loop;
         end loop;

         return Name (1 .. Nlen);

      <<Continue>>
         null;
      end loop;

      --  If we fall through, just use a peculiar unlikely name

      return ("Qwertyuiop");
   end Get_Ada_Main_Name;

   -------------------
   -- Get_Main_Name --
   -------------------

   function Get_Main_Name return String is
      Target         : constant String_Ptr := Target_Name;
      VxWorks_Target : constant Boolean :=
                         Target (Target'Last - 7 .. Target'Last) = "vxworks/";

   begin
      --  Explicit name given with -M switch

      if Bind_Alternate_Main_Name then
         return Alternate_Main_Name.all;

      --  Case of main program name to be used directly

      elsif VxWorks_Target then

         --  Get main program name

         Get_Name_String (Units.Table (First_Unit_Entry).Uname);

         --  If this is a child name, return only the name of the child,
         --  since we can't have dots in a nested program name. Note that
         --  we do not include the %b at the end of the unit name.

         for J in reverse 1 .. Name_Len - 3 loop
            if J = 1 or else Name_Buffer (J - 1) = '.' then
               return Name_Buffer (J .. Name_Len - 2);
            end if;
         end loop;

         raise Program_Error; -- impossible exit

      --  Case where "main" is to be used as default

      else
         return "main";
      end if;
   end Get_Main_Name;

   ----------------------
   -- Lt_Linker_Option --
   ----------------------

   function Lt_Linker_Option (Op1, Op2 : Natural) return Boolean is
   begin
      if Linker_Options.Table (Op1).Internal_File
           /=
         Linker_Options.Table (Op2).Internal_File
      then
         return Linker_Options.Table (Op1).Internal_File
                  <
                 Linker_Options.Table (Op2).Internal_File;
      else
         if Units.Table (Linker_Options.Table (Op1).Unit).Elab_Position
              /=
            Units.Table (Linker_Options.Table (Op2).Unit).Elab_Position
         then
            return Units.Table (Linker_Options.Table (Op1).Unit).Elab_Position
                     >
                   Units.Table (Linker_Options.Table (Op2).Unit).Elab_Position;

         else
            return Linker_Options.Table (Op1).Original_Pos
                     <
                   Linker_Options.Table (Op2).Original_Pos;
         end if;
      end if;
   end Lt_Linker_Option;

   ------------------------
   -- Move_Linker_Option --
   ------------------------

   procedure Move_Linker_Option (From : Natural; To : Natural) is
   begin
      Linker_Options.Table (To) := Linker_Options.Table (From);
   end Move_Linker_Option;

   ----------------------------
   -- Resolve_Binder_Options --
   ----------------------------

   procedure Resolve_Binder_Options is
   begin
      for E in Elab_Order.First .. Elab_Order.Last loop
         Get_Name_String (Units.Table (Elab_Order.Table (E)).Uname);

         --  The procedure of looking for specific packages and setting
         --  flags is very wrong, but there isn't a good alternative at
         --  this time.

         if Name_Buffer (1 .. 19) = "system.os_interface" then
            With_GNARL := True;
         end if;

         if Hostparm.OpenVMS and then Name_Buffer (1 .. 3) = "dec" then
            With_DECGNAT := True;
         end if;
      end loop;
   end Resolve_Binder_Options;

   --------------
   -- Set_Char --
   --------------

   procedure Set_Char (C : Character) is
   begin
      Last := Last + 1;
      Statement_Buffer (Last) := C;
   end Set_Char;

   -------------
   -- Set_Int --
   -------------

   procedure Set_Int (N : Int) is
   begin
      if N < 0 then
         Set_String ("-");
         Set_Int (-N);

      else
         if N > 9 then
            Set_Int (N / 10);
         end if;

         Last := Last + 1;
         Statement_Buffer (Last) :=
           Character'Val (N mod 10 + Character'Pos ('0'));
      end if;
   end Set_Int;

   ---------------------------
   -- Set_Main_Program_Name --
   ---------------------------

   procedure Set_Main_Program_Name is
   begin
      --  Note that name has %b on the end which we ignore

      --  First we output the initial _ada_ since we know that the main
      --  program is a library level subprogram.

      Set_String ("_ada_");

      --  Copy name, changing dots to double underscores

      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) = '.' then
            Set_String ("__");
         else
            Set_Char (Name_Buffer (J));
         end if;
      end loop;
   end Set_Main_Program_Name;

   ---------------------
   -- Set_Name_Buffer --
   ---------------------

   procedure Set_Name_Buffer is
   begin
      for J in 1 .. Name_Len loop
         Set_Char (Name_Buffer (J));
      end loop;
   end Set_Name_Buffer;

   ----------------
   -- Set_String --
   ----------------

   procedure Set_String (S : String) is
   begin
      Statement_Buffer (Last + 1 .. Last + S'Length) := S;
      Last := Last + S'Length;
   end Set_String;

   -------------------
   -- Set_Unit_Name --
   -------------------

   procedure Set_Unit_Name is
   begin
      for J in 1 .. Name_Len - 2 loop
         if Name_Buffer (J) /= '.' then
            Set_Char (Name_Buffer (J));
         else
            Set_String ("__");
         end if;
      end loop;
   end Set_Unit_Name;

   ---------------------
   -- Set_Unit_Number --
   ---------------------

   procedure Set_Unit_Number (U : Unit_Id) is
      Num_Units : constant Nat := Nat (Units.Table'Last) - Nat (Unit_Id'First);
      Unum      : constant Nat := Nat (U) - Nat (Unit_Id'First);

   begin
      if Num_Units >= 10 and then Unum < 10 then
         Set_Char ('0');
      end if;

      if Num_Units >= 100 and then Unum < 100 then
         Set_Char ('0');
      end if;

      Set_Int (Unum);
   end Set_Unit_Number;

   ------------
   -- Tab_To --
   ------------

   procedure Tab_To (N : Natural) is
   begin
      while Last < N loop
         Set_Char (' ');
      end loop;
   end Tab_To;

   -----------
   -- Value --
   -----------

   function Value (chars : chars_ptr) return String is
      function Strlen (chars : chars_ptr) return Natural;
      pragma Import (C, Strlen);

   begin
      if chars = Null_Address then
         return "";

      else
         declare
            subtype Result_Type is String (1 .. Strlen (chars));

            Result : Result_Type;
            for Result'Address use chars;

         begin
            return Result;
         end;
      end if;
   end Value;

   ----------------------
   -- Write_Info_Ada_C --
   ----------------------

   procedure Write_Info_Ada_C (Ada : String; C : String; Common : String) is
   begin
      if Ada_Bind_File then
         declare
            S : String (1 .. Ada'Length + Common'Length);

         begin
            S (1 .. Ada'Length) := Ada;
            S (Ada'Length + 1 .. S'Length) := Common;
            WBI (S);
         end;

      else
         declare
            S : String (1 .. C'Length + Common'Length);

         begin
            S (1 .. C'Length) := C;
            S (C'Length + 1 .. S'Length) := Common;
            WBI (S);
         end;
      end if;
   end Write_Info_Ada_C;

   ----------------------------
   -- Write_Statement_Buffer --
   ----------------------------

   procedure Write_Statement_Buffer is
   begin
      WBI (Statement_Buffer (1 .. Last));
      Last := 0;
   end Write_Statement_Buffer;

   procedure Write_Statement_Buffer (S : String) is
   begin
      Set_String (S);
      Write_Statement_Buffer;
   end Write_Statement_Buffer;

end Bindgen;
