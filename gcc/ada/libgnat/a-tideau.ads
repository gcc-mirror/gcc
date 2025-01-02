------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . T E X T _ I O . D E C I M A L _ A U X               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This package contains the implementation for Ada.Text_IO.Decimal_IO. The
--  routines in this package are identical semantically to those in Decimal_IO,
--  except that the default parameters have been removed because they are
--  supplied explicitly by the calls from within these units, and there is an
--  additional Scale parameter giving the value of Num'Scale. In addition the
--  Get routines return the value rather than store it in an Out parameter.

private generic
   type Int is range <>;

   with function Scan
     (Str   : String;
      Ptr   : not null access Integer;
      Max   : Integer;
      Scale : Integer) return Int;

   with procedure Set_Image
     (V     : Int;
      S     : in out String;
      P     : in out Natural;
      Scale : Integer;
      Fore  : Natural;
      Aft   : Natural;
      Exp   : Natural);

package Ada.Text_IO.Decimal_Aux is

   function Get
     (File  : File_Type;
      Width : Field;
      Scale : Integer) return Int;

   procedure Put
     (File  : File_Type;
      Item  : Int;
      Fore  : Field;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer);

   function Gets
     (From  : String;
      Last  : out Positive;
      Scale : Integer) return Int;

   procedure Puts
     (To    : out String;
      Item  : Int;
      Aft   : Field;
      Exp   : Field;
      Scale : Integer);

end Ada.Text_IO.Decimal_Aux;
