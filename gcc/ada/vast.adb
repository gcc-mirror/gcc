------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                 V A S T                                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2020-2025, Free Software Foundation, Inc.      --
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

with Atree; use Atree;
with Debug;
with Debug_A; use Debug_A;
with Lib; use Lib;
with Namet; use Namet;
with Output; use Output;
with Opt; use Opt;
with Sinfo.Nodes; use Sinfo.Nodes;
with Einfo.Entities; use Einfo.Entities;
with Types; use Types;

package body VAST is

   Force_Enable_VAST : constant Boolean := False;
   --  Normally, VAST is enabled by the the -gnatd_V switch.
   --  To force it to be enabled independent of any switches,
   --  change the above to True.

   function Do_Node (N : Node_Id) return Traverse_Result;
   procedure Traverse is new Traverse_Proc (Do_Node);
   --  Do VAST checking on a tree of nodes

   procedure Do_Unit (U : Unit_Number_Type);
   --  Call Do_Node on the root node of a compilation unit

   ------------------
   -- Do_Node --
   ------------------

   function Do_Node (N : Node_Id) return Traverse_Result is
   begin
      Debug_A_Entry ("do ", N);

      case Nkind (N) is
         when N_Unused_At_Start | N_Unused_At_End =>
            pragma Assert (False);

         when N_Entity =>
            case Ekind (N) is
               when others =>
                  null; -- more to be done here
            end case;

         when others =>
            null; -- more to be done here
      end case;

      Debug_A_Exit ("do ", N, "  (done)");
      return OK;
   end Do_Node;

   ------------------
   -- Do_Unit --
   ------------------

   procedure Do_Unit (U : Unit_Number_Type) is
      Root : constant Node_Id := Cunit (U);
      U_Name : constant Unit_Name_Type := Unit_Name (U);
      U_Name_S : constant String :=
        (if U_Name = No_Unit_Name then "<No_Unit_Name>"
         else Get_Name_String (U_Name));
      Predef : constant String :=
        (if Is_Predefined_Unit (U) then " (predef)"
         elsif Is_Internal_Unit (U) then " (gnat)"
         else "");
      Msg : constant String :=
        "VAST for unit" & U'Img & " " & U_Name_S & Predef;

      Is_Preprocessing_Dependency : constant Boolean :=
        U_Name = No_Unit_Name;
      --  True if this is a bogus unit added by Add_Preprocessing_Dependency.
      --  ???Not sure what that's about.
      pragma Assert (No (Root) = Is_Preprocessing_Dependency);
      --  There should be no Cunit (only) for these bogus units.
   begin
      Write_Line (Msg);

      if Is_Preprocessing_Dependency then
         Write_Line ("Skipping preprocessing dependency");
         return;
      end if;

      pragma Assert (Present (Root));
      Traverse (Root);
      Write_Line (Msg & "  (done)");
   end Do_Unit;

   ----------------
   -- Check_Tree --
   ----------------

   procedure VAST is
      use Debug;
   begin
      if Operating_Mode /= Generate_Code then
         return;
      end if;

      --  If -gnatd_W (VAST in verbose mode) is enabled, then that should imply
      --  -gnatd_V (enable VAST). In addition, we use the Debug_A routines to
      --  print debugging information, so enable -gnatda.

      if Debug_Flag_Underscore_WW then
         Debug_Flag_Underscore_VV := True;
         Debug_Flag_A := True;
      end if;

      if not Debug_Flag_Underscore_VV and then not Force_Enable_VAST then
         return;
      end if;

      if not Debug_Flag_Underscore_WW then
         Set_Special_Output (Ignore_Output'Access);
      end if;
      Write_Line ("VAST");

      pragma Assert (Serious_Errors_Detected = 0);

      Write_Line ("VAST checking" & Last_Unit'Img & " units");
      for U in Main_Unit .. Last_Unit loop
         Do_Unit (U);
      end loop;

      Write_Line ("VAST done.");
      Cancel_Special_Output;
   end VAST;

end VAST;
