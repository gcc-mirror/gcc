------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                          V X L I N K . M A I N                           --
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

--  VxLink is a helper tool used as a wrapper around g++/gcc to build VxWorks
--  DKM (Downloadable Kernel Modules).
--  Such DKM is a partially linked object that contains entry points for
--  constructors and destructors. This tool thus uses g++ to generate an
--  intermediate partially linked object, retrieves the list of constructors
--  and destructors in it and produces a C file that lists those ctors/dtors
--  in a way that is understood be VxWorks kernel. It then links this file
--  with the intermediate object to produce a valid DKM.

pragma Ada_2012;

with GNAT.OS_Lib; use GNAT.OS_Lib;

with VxLink.Link; use VxLink.Link;
with VxLink.Bind; use VxLink.Bind;

procedure VxLink.Main is
   Linker  : VxLink_Linker;
   Binder  : VxLink_Binder;
   VSB_Dir : String_Access := Getenv ("VSB_DIR");
begin
   Initialize (Linker);

   if Is_Error_State then
      return;
   end if;

   Do_Initial_Link (Linker);

   if Is_Error_State then
      return;
   end if;

   if not Needs_CDtor (Linker) then
      --  Initial link is enough, let's return
      return;
   end if;

   if VSB_Dir /= null and then VSB_Dir'Length > 0 then
      declare
         DKM_Tag_File : constant String :=
                          Normalize_Pathname
                            ("krnl/tags/dkm.tags", VSB_Dir.all);
      begin
         if Is_Regular_File (DKM_Tag_File) then
            Parse_Tag_File (Binder, DKM_Tag_File);
         end if;
      end;
   end if;

   Initialize (Binder, Object_File => Partial_Object (Linker));
   Emit_CTDT (Binder, Namespace => Namespace (Linker));

   Do_Final_Link (Linker, CTDT_File (Binder));
   Free (VSB_Dir);
end VxLink.Main;
