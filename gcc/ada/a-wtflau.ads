------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           A D A . W I D E _ T E X T _ I O . F L O A T _ A U X            --
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

--  This package contains the routines for Ada.Wide_Text_IO.Float_IO that
--  are shared among separate instantiations of this package. The routines
--  in this package are identical semantically to those in Float_IO itself,
--  except that generic parameter Num has been replaced by Long_Long_Float,
--  and the default parameters have been removed because they are supplied
--  explicitly by the calls from within the generic template. This package
--  is also used by Ada.Wide_Text_IO.Fixed_IO, Ada.Wide_Text_IO.Decimal_IO.

private package Ada.Wide_Text_IO.Float_Aux is

   procedure Load_Real
     (File : in File_Type;
      Buf  : out String;
      Ptr  : in out Natural);
   --  This is an auxiliary routine that is used to load a possibly signed
   --  real literal value from the input file into Buf, starting at Ptr + 1.

   procedure Get
     (File  : in File_Type;
      Item  : out Long_Long_Float;
      Width : in Field);

   procedure Gets
     (From : in String;
      Item : out Long_Long_Float;
      Last : out Positive);

   procedure Put
     (File : in File_Type;
      Item : in Long_Long_Float;
      Fore : in Field;
      Aft  : in Field;
      Exp  : in Field);

   procedure Puts
     (To   : out String;
      Item : in Long_Long_Float;
      Aft  : in Field;
      Exp  : in Field);

end Ada.Wide_Text_IO.Float_Aux;
