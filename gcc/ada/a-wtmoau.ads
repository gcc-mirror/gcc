------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . W I D E _ T E X T _ I O . M O D U L A R _ A U X          --
--                                                                          --
--                                 S p e c                                  --
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
     (File  : in File_Type;
      Item  : out U.Unsigned;
      Width : in Field);

   procedure Get_LLU
     (File  : in File_Type;
      Item  : out U.Long_Long_Unsigned;
      Width : in Field);

   procedure Gets_Uns
     (From : in String;
      Item : out U.Unsigned;
      Last : out Positive);

   procedure Gets_LLU
     (From : in String;
      Item : out U.Long_Long_Unsigned;
      Last : out Positive);

   procedure Put_Uns
     (File  : in File_Type;
      Item  : in U.Unsigned;
      Width : in Field;
      Base  : in Number_Base);

   procedure Put_LLU
     (File  : in File_Type;
      Item  : in U.Long_Long_Unsigned;
      Width : in Field;
      Base  : in Number_Base);

   procedure Puts_Uns
     (To   : out String;
      Item : in U.Unsigned;
      Base : in Number_Base);

   procedure Puts_LLU
     (To   : out String;
      Item : in U.Long_Long_Unsigned;
      Base : in Number_Base);

end Ada.Wide_Text_IO.Modular_Aux;
