------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                              G N A T . I O                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--           Copyright (C) 1995-2001 Ada Core Technologies, Inc.            --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT is maintained by Ada Core Technologies Inc (http://www.gnat.com).   --
--                                                                          --
------------------------------------------------------------------------------

--  A simple preelaborable subset of Text_IO capabilities

--  A simple text I/O package that can be used for simple I/O functions in
--  user programs as required. This package is also preelaborated, unlike
--  Text_IO, and can thus be with'ed by preelaborated library units.

--  Note that Data_Error is not raised by these subprograms for bad data.
--  If such checks are needed then the regular Text_IO package must be used.

package GNAT.IO is
pragma Preelaborate (IO);

   type File_Type is limited private;
   --  Specifies file to be used (the only possibilities are Standard_Output
   --  and Standard_Error). There is no Create or Open facility that would
   --  allow more general use of file names.

   function Standard_Output return File_Type;
   function Standard_Error  return File_Type;
   --  These functions are the only way to get File_Type values

   procedure Get (X : out Integer);
   procedure Get (C : out Character);
   procedure Get_Line (Item : in out String; Last : out Natural);
   --  These routines always read from Standard_Input

   procedure Put (File : File_Type; X : Integer);
   procedure Put (X : Integer);
   --  Output integer to specified file, or to current output file, same
   --  output as if Ada.Text_IO.Integer_IO had been instantiated for Integer.

   procedure Put (File : File_Type; C : Character);
   procedure Put (C : Character);
   --  Output character to specified file, or to current output file

   procedure Put (File : File_Type; S : String);
   procedure Put (S : String);
   --  Output string to specified file, or to current output file

   procedure Put_Line (File : File_Type; S : String);
   procedure Put_Line (S : String);
   --  Output string followed by new line to specified file, or to
   --  current output file.

   procedure New_Line (File : File_Type; Spacing : Positive := 1);
   procedure New_Line (Spacing : Positive := 1);
   --  Output new line character to specified file, or to current output file

   procedure Set_Output (File : File_Type);
   --  Set current output file, default is Standard_Output if no call to
   --  Set_Output is made.

private
   type File_Type is (Stdout, Stderr);
   --  Stdout = Standard_Output, Stderr = Standard_Error

   pragma Inline (Standard_Error);
   pragma Inline (Standard_Output);

end GNAT.IO;
