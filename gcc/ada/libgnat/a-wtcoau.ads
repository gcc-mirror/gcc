------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . W I D E _ T E X T _ I O . C O M P L E X _ A U X          --
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

--  This package contains the routines for Ada.Wide_Text_IO.Complex_IO that
--  are shared among separate instantiations of this package. The routines
--  in this package are identical semantically to those in Complex_IO itself,
--  except that the generic parameter Complex has been replaced by separate
--  real and imaginary values of type Long_Long_Float, and default parameters
--  have been removed because they are supplied explicitly by the calls from
--  within the generic template.

package Ada.Wide_Text_IO.Complex_Aux is

   procedure Get
     (File  : File_Type;
      ItemR : out Long_Long_Float;
      ItemI : out Long_Long_Float;
      Width : Field);

   procedure Gets
     (From  : String;
      ItemR : out Long_Long_Float;
      ItemI : out Long_Long_Float;
      Last  : out Positive);

   procedure Put
     (File  : File_Type;
      ItemR : Long_Long_Float;
      ItemI : Long_Long_Float;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field);

   procedure Puts
     (To    : out String;
      ItemR : Long_Long_Float;
      ItemI : Long_Long_Float;
      Aft   : Field;
      Exp   : Field);

end Ada.Wide_Text_IO.Complex_Aux;
