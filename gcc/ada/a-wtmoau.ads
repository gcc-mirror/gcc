------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . W I D E _ T E X T _ I O . M O D U L A R _ A U X          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
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

--  This package contains the routines for Ada.Wide_Text_IO.Modular_IO that
--  are shared among separate instantiations of this package. The routines
--  in this package are identical semantically to those in Modular_IO itself,
--  except that the generic parameter Num has been replaced by Unsigned or
--  Long_Long_Unsigned, and the default parameters have been removed because
--  they are supplied explicitly by the calls from within the generic template.

with System.Unsigned_Types;

private package Ada.Wide_Text_IO.Modular_Aux is

   package U renames System.Unsigned_Types;

   procedure Get_Uns
     (File  : File_Type;
      Item  : out U.Unsigned;
      Width : Field);

   procedure Get_LLU
     (File  : File_Type;
      Item  : out U.Long_Long_Unsigned;
      Width : Field);

   procedure Gets_Uns
     (From : String;
      Item : out U.Unsigned;
      Last : out Positive);

   procedure Gets_LLU
     (From : String;
      Item : out U.Long_Long_Unsigned;
      Last : out Positive);

   procedure Put_Uns
     (File  : File_Type;
      Item  : U.Unsigned;
      Width : Field;
      Base  : Number_Base);

   procedure Put_LLU
     (File  : File_Type;
      Item  : U.Long_Long_Unsigned;
      Width : Field;
      Base  : Number_Base);

   procedure Puts_Uns
     (To   : out String;
      Item : U.Unsigned;
      Base : Number_Base);

   procedure Puts_LLU
     (To   : out String;
      Item : U.Long_Long_Unsigned;
      Base : Number_Base);

end Ada.Wide_Text_IO.Modular_Aux;
