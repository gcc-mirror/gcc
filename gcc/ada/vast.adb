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

pragma Unsuppress (All_Checks);
pragma Assertion_Policy (Check);
--  Enable checking. This isn't really necessary, but it might come in handy if
--  we want to run VAST with a compiler built without checks. Anyway, it's
--  harmless, because VAST is not run by default.

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
   Print_Disabled_Failing_Checks : constant Boolean := True;
   --  False means disabled checks are silent; True means we print a message
   --  (but still don't raise VAST_Failure).

   type Check_Enum is (Check_Other, Check_Itype_Parents, Check_Error_Nodes);
   Enabled_Checks : constant array (Check_Enum) of Boolean :=
--     (Check_Other => True, others => False);
     (others => True);
--     (Check_Itype_Parents => False, -- this one fails in bootstrap!
--      others => True);
   --  Passing checks are Check_Other, which should always be enabled.
   --  Currently-failing checks are different enumerals in Check_Enum,
   --  which can be disabled individually until we fix the bugs, or enabled
   --  when debugging particular bugs. Pass a nondefault Check_Enum to
   --  Assert in order to deal with bugs we have not yet fixed,
   --  and play around with the value of Enabled_Checks above
   --  for testing and debugging.
   --
   --  Note: Once a bug is fixed, and the check passes reliably, we may choose
   --  to remove that check from Check_Enum and use Check_Other instead.

   procedure Assert
     (Condition : Boolean;
      Check : Check_Enum := Check_Other;
      Detail : String := "");
   --  Check that the Condition is True, and raise an exception otherwise.
   --  Check enables/disables the checking, according to Enabled_Checks above,
   --  and is printed on failure. Detail is an additional error message,
   --  also printed on failure.

   function Do_Node (N : Node_Id) return Traverse_Result;
   procedure Traverse is new Traverse_Proc (Do_Node);
   --  Do VAST checking on a tree of nodes

   procedure Do_Unit (U : Unit_Number_Type);
   --  Call Do_Node on the root node of a compilation unit

   ------------
   -- Assert --
   ------------

   VAST_Failure : exception;

   procedure Assert
     (Condition : Boolean;
      Check : Check_Enum := Check_Other;
      Detail : String := "")
   is
   begin
      if not Condition then
         declare
            Part1 : constant String := "VAST fail";
            Part2 : constant String :=
              (if Check = Check_Other then "" else ": " & Check'Img);
            Part3 : constant String :=
              (if Detail = "" then "" else " -- " & Detail);
            Message : constant String := Part1 & Part2 & Part3;
         begin
            if Enabled_Checks (Check) or else Print_Disabled_Failing_Checks
            then
               --  ???This Special_Output business is kind of ugly.
               --  We can do better.
               Cancel_Special_Output;
               Write_Line (Message);
               Set_Special_Output (Ignore_Output'Access);
            end if;

            if Enabled_Checks (Check) then
               raise VAST_Failure with Message;
            end if;
         end;
      end if;
   end Assert;

   -------------
   -- Do_Node --
   -------------

   function Do_Node (N : Node_Id) return Traverse_Result is
   begin
      Debug_A_Entry ("do ", N);

      case Nkind (N) is
         when N_Unused_At_Start | N_Unused_At_End =>
            Assert (False);

         when N_Error =>
            --  VAST doesn't do anything when Serious_Errors_Detected > 0 (at
            --  least for now), so we shouldn't encounter any N_Error nodes.
            Assert (False, Check_Error_Nodes);

         when N_Entity =>
            case Ekind (N) is
               when others =>
                  null; -- more to be done here
            end case;

         when others =>
            null; -- more to be done here
      end case;

      --  Check that N has a Parent, except in certain cases:

      if Nkind (N) = N_Compilation_Unit then
         Assert (No (Parent (N)));
         --  The root of each unit should not have a parent
      elsif N in N_Entity_Id and then Is_Itype (N) then
         Assert (No (Parent (N)), Check_Itype_Parents);
         --  Itypes should not have a parent
      else
         if Nkind (N) = N_Error then
            Assert (False, Check_Error_Nodes);
            --  The error node has no parent, but we shouldn't even be seeing
            --  error nodes in VAST at all. See "when N_Error" above.
         else
            Assert (Present (Parent (N)), Detail => "missing parent");
            --  All other nodes should have a parent
         end if;
      end if;

      Debug_A_Exit ("do ", N, "  (done)");
      return OK;
   end Do_Node;

   -------------
   -- Do_Unit --
   -------------

   procedure Do_Unit (U : Unit_Number_Type) is
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
      --  ???Not sure what that's about, but these units have no name and
      --  no associated tree, so we had better not try to walk those trees.

      Root : constant Node_Id := Cunit (U);
   begin
      Assert (No (Root) = Is_Preprocessing_Dependency);
      --  All compilation units except these bogus ones should have a Cunit.

      Write_Line (Msg);

      if Is_Preprocessing_Dependency then
         Write_Line ("Skipping preprocessing dependency");
         return;
      end if;

      Assert (Present (Root));
      Traverse (Root);
      Write_Line (Msg & "  (done)");
   end Do_Unit;

   ----------------
   -- Check_Tree --
   ----------------

   procedure VAST is
      pragma Assert (Expander_Active = (Operating_Mode = Generate_Code));
      --  ???So why do we need both Operating_Mode and Expander_Active?
      use Debug;
   begin
      --  Do nothing if we're not calling the back end; the main point of VAST
      --  is to protect against code-generation bugs. This includes the
      --  case where legality errors were detected; the tree is known to be
      --  malformed in some error cases.

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

      --  Do nothing if VAST is disabled

      if not (Debug_Flag_Underscore_VV or Force_Enable_VAST) then
         return;
      end if;

      --  Turn off output unless verbose mode is enabled

      if not Debug_Flag_Underscore_WW then
         Set_Special_Output (Ignore_Output'Access);
      end if;
      Write_Line ("VAST");

      --  Operating_Mode = Generate_Code implies there are no legality errors:

      Assert (Serious_Errors_Detected = 0);

      Write_Line ("VAST checking" & Last_Unit'Img & " units");
      for U in Main_Unit .. Last_Unit loop
         Do_Unit (U);
      end loop;

      Write_Line ("VAST done.");
      Cancel_Special_Output;
   end VAST;

end VAST;
