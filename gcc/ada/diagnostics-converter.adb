------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                D I A G N O S T I C S . C O N V E R T E R                 --
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
with Erroutc; use Erroutc;
with Debug;   use Debug;
with Diagnostics.Repository;        use Diagnostics.Repository;
with Diagnostics.SARIF_Emitter;     use Diagnostics.SARIF_Emitter;
with Diagnostics.Switch_Repository; use Diagnostics.Switch_Repository;
with Opt;    use Opt;
with Osint;  use Osint;
with Output; use Output;
use Diagnostics.Diagnostics_Lists;
with System.OS_Lib; use System.OS_Lib;

package body Diagnostics.Converter is

   function Convert (E_Id : Error_Msg_Id) return Diagnostic_Type;

   function Convert_Sub_Diagnostic
     (E_Id : Error_Msg_Id) return Sub_Diagnostic_Type;

   function Get_Warning_Kind (E_Msg : Error_Msg_Object) return Diagnostic_Kind
   is (if E_Msg.Warn_Chr = "* " then Restriction_Warning
       elsif E_Msg.Warn_Chr = "? " then Default_Warning
       elsif E_Msg.Warn_Chr = "  " then Tagless_Warning
       else Warning);
   --  NOTE: Some messages have both info and warning set to true. The old
   --  printer added the warning switch label but treated the message as
   --  an info message.

   function Get_Diagnostics_Kind (E_Msg : Error_Msg_Object)
                                  return Diagnostic_Kind
   is (if E_Msg.Kind = Erroutc.Warning then Get_Warning_Kind (E_Msg)
      elsif E_Msg.Kind = Erroutc.Style then Style
      elsif E_Msg.Kind = Erroutc.Info then Info
      elsif E_Msg.Kind = Erroutc.Non_Serious_Error then Non_Serious_Error
      else Error);

   -----------------------------------
   -- Convert_Errors_To_Diagnostics --
   -----------------------------------

   procedure Convert_Errors_To_Diagnostics
   is
      E : Error_Msg_Id;
   begin
      E := First_Error_Msg;
      while E /= No_Error_Msg loop

         if not Errors.Table (E).Deleted
           and then not Errors.Table (E).Msg_Cont
         then

            --  We do not need to update the count of converted error messages
            --  since they are accounted for in their creation.

            Record_Diagnostic (Convert (E), Update_Count => False);
         end if;

         E := Errors.Table (E).Next;
      end loop;

   end Convert_Errors_To_Diagnostics;

   ----------------------------
   -- Convert_Sub_Diagnostic --
   ----------------------------

   function Convert_Sub_Diagnostic
     (E_Id : Error_Msg_Id) return Sub_Diagnostic_Type
   is
      E_Msg : constant Error_Msg_Object := Errors.Table (E_Id);
      D : Sub_Diagnostic_Type;
   begin
      D.Message := E_Msg.Text;

      --  All converted sub-diagnostics are continuations. When emitted they
      --  shall be printed with the same kind token as the main diagnostic.
      D.Kind := Continuation;

      Add_Location (D,
        Primary_Labeled_Span
          (if E_Msg.Insertion_Sloc /= No_Location
           then To_Span (E_Msg.Insertion_Sloc)
           else E_Msg.Sptr));

      if E_Msg.Optr.Ptr /= E_Msg.Sptr.Ptr then
         Add_Location (D, Secondary_Labeled_Span (E_Msg.Optr));
      end if;

      return D;
   end Convert_Sub_Diagnostic;

   -------------
   -- Convert --
   -------------

   function Convert (E_Id : Error_Msg_Id) return Diagnostic_Type is

      E_Next_Id : Error_Msg_Id;

      E_Msg : constant Error_Msg_Object := Errors.Table (E_Id);
      D : Diagnostic_Type;
   begin
      D.Message := E_Msg.Text;

      D.Kind := Get_Diagnostics_Kind (E_Msg);

      if E_Msg.Kind in Erroutc.Warning | Erroutc.Style | Erroutc.Info then
         D.Switch := Get_Switch_Id (E_Msg);
      end if;

      D.Warn_Err := E_Msg.Warn_Err;

      --  Convert the primary location

      Add_Location (D, Primary_Labeled_Span (E_Msg.Sptr));

      --  Convert the secondary location if it is different from the primary

      if E_Msg.Optr.Ptr /= E_Msg.Sptr.Ptr then
         Add_Location (D, Secondary_Labeled_Span (E_Msg.Optr));
      end if;

      E_Next_Id := Errors.Table (E_Id).Next;
      while E_Next_Id /= No_Error_Msg
        and then Errors.Table (E_Next_Id).Msg_Cont
      loop
         Add_Sub_Diagnostic (D, Convert_Sub_Diagnostic (E_Next_Id));
         E_Next_Id := Errors.Table (E_Next_Id).Next;
      end loop;

      return D;
   end Convert;

   ----------------------
   -- Emit_Diagnostics --
   ----------------------

   procedure Emit_Diagnostics is
      D : Diagnostic_Type;

      It : Iterator := Iterate (All_Diagnostics);

      Sarif_File_Name : constant String :=
        Get_First_Main_File_Name & ".gnat.sarif";

      Switches_File_Name : constant String := "gnat_switches.json";

      Diagnostics_File_Name : constant String := "gnat_diagnostics.json";

      Dummy : Boolean;
   begin
      if Opt.SARIF_Output then
         Set_Standard_Error;

         Print_SARIF_Report (All_Diagnostics);

         Set_Standard_Output;
      elsif Opt.SARIF_File then
         Delete_File (Sarif_File_Name, Dummy);
         declare
            Output_FD : constant File_Descriptor :=
              Create_New_File
                (Sarif_File_Name,
                 Fmode => Text);

         begin
            Set_Output (Output_FD);

            Print_SARIF_Report (All_Diagnostics);

            Set_Standard_Output;

            Close (Output_FD);
         end;
      else
         Set_Standard_Error;

         while Has_Next (It) loop
            Next (It, D);

            Print_Diagnostic (D);
         end loop;

         Set_Standard_Output;
      end if;

      if Debug_Flag_Underscore_EE then

         --  Print the switch repository to a file

         Delete_File (Switches_File_Name, Dummy);
         declare
            Output_FD : constant File_Descriptor :=
              Create_New_File
                (Switches_File_Name,
                 Fmode => Text);

         begin
            Set_Output (Output_FD);

            Print_Switch_Repository;

            Set_Standard_Output;

            Close (Output_FD);
         end;

         --  Print the diagnostics repository to a file

         Delete_File (Diagnostics_File_Name, Dummy);
         declare
            Output_FD : constant File_Descriptor :=
              Create_New_File
                (Diagnostics_File_Name,
                 Fmode => Text);

         begin
            Set_Output (Output_FD);

            Print_Diagnostic_Repository;

            Set_Standard_Output;

            Close (Output_FD);
         end;
      end if;

      Destroy (All_Diagnostics);
   end Emit_Diagnostics;

end Diagnostics.Converter;
