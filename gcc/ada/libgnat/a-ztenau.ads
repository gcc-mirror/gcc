------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                  ADA.WIDE_WIDE_TEXT_IO.ENUMERATION_AUX                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  This package contains the routines for Ada.Wide_Wide_Text_IO.Enumeration_IO
--  that are shared among separate instantiations.

private package Ada.Wide_Wide_Text_IO.Enumeration_Aux is

   procedure Get_Enum_Lit
     (File   : File_Type;
      Buf    : out Wide_Wide_String;
      Buflen : out Natural);
   --  Reads an enumeration literal value from the file, folds to upper case,
   --  and stores the result in Buf, setting Buflen to the number of stored
   --  characters (Buf has a lower bound of 1). If more than Buflen characters
   --  are present in the literal, Data_Error is raised.

   procedure Scan_Enum_Lit
     (From  : Wide_Wide_String;
      Start : out Natural;
      Stop  : out Natural);
   --  Scans an enumeration literal at the start of From, skipping any leading
   --  spaces. Sets Start to the first character, Stop to the last character.
   --  Raises End_Error if no enumeration literal is found.

   procedure Put
     (File  : File_Type;
      Item  : Wide_Wide_String;
      Width : Field;
      Set   : Type_Set);
   --  Outputs the enumeration literal image stored in Item to the given File,
   --  using the given Width and Set parameters (Item is always in upper case).

   procedure Puts
     (To   : out Wide_Wide_String;
      Item : Wide_Wide_String;
      Set  : Type_Set);
   --  Stores the enumeration literal image stored in Item to the string To,
   --  padding with trailing spaces if necessary to fill To. Set is used to

end Ada.Wide_Wide_Text_IO.Enumeration_Aux;
