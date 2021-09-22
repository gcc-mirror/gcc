------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                      ADA.STRINGS.TEXT_BUFFERS.FILES                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020-2021, Free Software Foundation, Inc.       --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body Ada.Strings.Text_Buffers.Files is

   procedure Put_UTF_8_Implementation
     (Buffer : in out Root_Buffer_Type'Class;
      Item : UTF_Encoding.UTF_8_String) is
      Result : Integer;
   begin
      Result := OS.Write (File_Buffer (Buffer).FD,
                          Item (Item'First)'Address,
                          Item'Length);
      if Result /= Item'Length then
         raise Program_Error with OS.Errno_Message;
      end if;
   end Put_UTF_8_Implementation;

   function Create_From_FD
     (FD                      : System.OS_Lib.File_Descriptor;
      Close_Upon_Finalization : Boolean := True) return File_Buffer
   is
      use OS;
   begin
      if FD = Invalid_FD then
         raise Program_Error with OS.Errno_Message;
      end if;
      return Result : File_Buffer do
         Result.FD := FD;
         Result.Close_Upon_Finalization := Close_Upon_Finalization;
      end return;
   end Create_From_FD;

   function Create_File (Name : String) return File_Buffer is
   begin
      return Create_From_FD (OS.Create_File (Name, Fmode => OS.Binary));
   end Create_File;

   procedure Finalize (Ref : in out Self_Ref) is
      Success : Boolean;
      use OS;
   begin
      if Ref.Self.FD /= OS.Invalid_FD
        and then Ref.Self.Close_Upon_Finalization
      then
         Close (Ref.Self.FD, Success);
         if not Success then
            raise Program_Error with OS.Errno_Message;
         end if;
      end if;
      Ref.Self.FD := OS.Invalid_FD;
   end Finalize;

end Ada.Strings.Text_Buffers.Files;
