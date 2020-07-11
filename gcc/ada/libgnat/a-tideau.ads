------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . T E X T _ I O . D E C I M A L _ A U X               --
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

--  This package contains the routines for Ada.Text_IO.Decimal_IO that are
--  shared among separate instantiations of this package. The routines in
--  the package are identical semantically to those declared in Text_IO,
--  except that default values have been supplied by the generic, and the
--  Num parameter has been replaced by Integer or Long_Long_Integer, with
--  an additional Scale parameter giving the value of Num'Scale. In addition
--  the Get routines return the value rather than store it in an Out parameter.

private package Ada.Text_IO.Decimal_Aux is

   function Get_Dec
     (File  : File_Type;
      Width : Field;
      Scale : Integer) return Integer;

   function Get_LLD
     (File  : File_Type;
      Width : Field;
      Scale : Integer) return Long_Long_Integer;

   procedure Put_Dec
     (File  : File_Type;
      Item  : Integer;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer);

   procedure Put_LLD
     (File  : File_Type;
      Item  : Long_Long_Integer;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer);

   function Gets_Dec
     (From  : String;
      Last  : not null access Positive;
      Scale : Integer) return Integer;

   function Gets_LLD
     (From  : String;
      Last  : not null access Positive;
      Scale : Integer) return Long_Long_Integer;

   procedure Puts_Dec
     (To    : out String;
      Item  : Integer;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer);

   procedure Puts_LLD
     (To    : out String;
      Item  : Long_Long_Integer;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer);

end Ada.Text_IO.Decimal_Aux;
