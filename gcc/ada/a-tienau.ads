------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . T E X T _ I O . E N U M E R A T I O N _ A U X           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines for Ada.Text_IO.Enumeration_IO
--  that are shared among separate instantiations of this package.

private package Ada.Text_IO.Enumeration_Aux is

   procedure Get_Enum_Lit
     (File   : File_Type;
      Buf    : out String;
      Buflen : out Natural);
   --  Reads an enumeration literal value from the file, folds to upper case,
   --  and stores the result in Buf, setting Buflen to the number of stored
   --  characters (Buf has a lower bound of 1). If more than Buflen characters
   --  are present in the literal, Data_Error is raised.

   procedure Scan_Enum_Lit
     (From  : String;
      Start : out Natural;
      Stop  : out Natural);
   --  Scans an enumeration literal at the start of From, skipping any leading
   --  spaces. Sets Start to the first character, Stop to the last character.
   --  Raises End_Error if no enumeration literal is found.

   procedure Put
     (File  : File_Type;
      Item  : String;
      Width : Field;
      Set   : Type_Set);
   --  Outputs the enumeration literal image stored in Item to the given File,
   --  using the given Width and Set parameters (Item is always in upper case).

   procedure Puts
     (To    : out String;
      Item  : in String;
      Set   : Type_Set);
   --  Stores the enumeration literal image stored in Item to the string To,
   --  padding with trailing spaces if necessary to fill To. Set is used to

end Ada.Text_IO.Enumeration_Aux;
