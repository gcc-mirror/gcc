------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . W I D E _ W I D E _ T E X T _ I O . F L O A T _ A U X       --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

--  This package contains the routines for Ada.Wide_Wide_Text_IO.Float_IO that
--  are shared among separate instantiations of this package. The routines
--  in this package are identical semantically to those in Float_IO itself,
--  except that generic parameter Num has been replaced by Long_Long_Float,
--  and the default parameters have been removed because they are supplied
--  explicitly by the calls from within the generic template. Also used by
--  Ada.Wide_Wide_Text_IO.Fixed_IO, and by Ada.Wide_Wide_Text_IO.Decimal_IO.

private package Ada.Wide_Wide_Text_IO.Float_Aux is

   procedure Load_Real
     (File : File_Type;
      Buf  : out String;
      Ptr  : in out Natural);
   --  This is an auxiliary routine that is used to load a possibly signed
   --  real literal value from the input file into Buf, starting at Ptr + 1.

   procedure Get
     (File  : File_Type;
      Item  : out Long_Long_Float;
      Width : Field);

   procedure Gets
     (From : String;
      Item : out Long_Long_Float;
      Last : out Positive);

   procedure Put
     (File : File_Type;
      Item : Long_Long_Float;
      Fore : Field;
      Aft  : Field;
      Exp  : Field);

   procedure Puts
     (To   : out String;
      Item : Long_Long_Float;
      Aft  : Field;
      Exp  : Field);

end Ada.Wide_Wide_Text_IO.Float_Aux;
