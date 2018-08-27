------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          V X L I N K . L I N K                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2018, AdaCore                          --
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

pragma Ada_2012;

with Ada.Command_Line;          use Ada.Command_Line;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body VxLink.Link is

   Gcc : constant String := VxLink.Gcc;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Linker : out VxLink_Linker)
   is
      Leading        : Boolean := True;
      Next_Is_Object : Boolean := False;

   begin
      for J in 1 .. Ada.Command_Line.Argument_Count loop
         declare
            Arg : String renames Argument (J);
         begin
            if Next_Is_Object then
               Next_Is_Object     := False;
               Linker.Dest_Object := To_Unbounded_String (Arg);
               Leading            := False;

            elsif Argument (J) = "-o" then
               Next_Is_Object := True;

            elsif Argument (J) = "-noauto-register" then
               --  Filter out this argument, and do not generate _ctors/_dtors
               Linker.Add_CDtors := False;
            elsif Arg = "-v" and then not Is_Verbose then
               --  first -v means VxLink should be verbose, two -v passes -v to
               --  the linker.
               Set_Verbose (True);
            else
               if Arg = "-nostdlib" or Arg = "-nostartfiles" then
                  Linker.Add_CDtors := False;
               end if;

               if Leading then
                  Linker.Args_Leading.Append (Arg);
               else
                  Linker.Args_Trailing.Append (Arg);
               end if;
            end if;
         end;
      end loop;

      if Linker.Dest_Object = Null_Unbounded_String then
         Set_Error_State ("no output object is defined");
      elsif Linker.Add_CDtors then
         --  We'll need to create intermediate artefacts, so we'll use the
         --  destination object as base namespace just in case we have
         --  several link operations in the same directory
         declare
            Obj : constant String :=
                    Base_Name (To_String (Linker.Dest_Object));

         begin
            for J in reverse Obj'Range loop
               if Obj (J) = '.' then
                  Linker.Dest_Base :=
                    To_Unbounded_String (Obj (Obj'First .. J - 1));
                  exit;
               end if;
            end loop;

            Linker.Partial_Obj := Linker.Dest_Base & "-partial.o";
         end;
      end if;
   end Initialize;

   -----------------
   -- Needs_CDtor --
   -----------------

   function Needs_CDtor (Linker : VxLink_Linker) return Boolean is
   begin
      return Linker.Add_CDtors;
   end Needs_CDtor;

   --------------------
   -- Partial_Object --
   --------------------

   function Partial_Object (Linker : VxLink_Linker) return String is
   begin
      return To_String (Linker.Partial_Obj);
   end Partial_Object;

   ---------------
   -- Namespace --
   ---------------

   function Namespace (Linker : VxLink_Linker) return String is
   begin
      return To_String (Linker.Dest_Base);
   end Namespace;

   ---------------------
   -- Do_Initial_Link --
   ---------------------

   procedure Do_Initial_Link (Linker : VxLink_Linker)
   is
      Args : Arguments_List;
      Gxx_Path : constant String := Gxx;
   begin
      if Is_Error_State then
         return;
      end if;

      if Gxx_Path'Length /= 0 then
         Args.Append (Gxx);
      else
         Args.Append (Gcc);
      end if;
      Args.Append (Linker.Args_Leading);
      Args.Append ("-o");

      if Linker.Add_CDtors then
         Args.Append (To_String (Linker.Partial_Obj));
      else
         Args.Append (To_String (Linker.Dest_Object));
      end if;

      Args.Append (Linker.Args_Trailing);

      if not Linker.Add_CDtors then
         Args.Append ("-nostartfiles");
      end if;

      Run (Args);
   end Do_Initial_Link;

   -------------------
   -- Do_Final_Link --
   -------------------

   procedure Do_Final_Link
     (Linker   : VxLink_Linker;
      Ctdt_Obj : String)
   is
      Args : Arguments_List;
   begin
      if not Linker.Add_CDtors then
         return;
      end if;

      if Is_Error_State then
         return;
      end if;

      Args.Append (Gcc);
      Args.Append ("-nostdlib");
      Args.Append (Ctdt_Obj);
      Args.Append (To_String (Linker.Partial_Obj));
      Args.Append ("-o");
      Args.Append (To_String (Linker.Dest_Object));

      Run (Args);
   end Do_Final_Link;

end VxLink.Link;
