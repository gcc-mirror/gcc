------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--    A D A . W I D E _ W I D E _ T E X T _ I O . I N T E G E R  _ A U X    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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

--  This package contains implementation for Ada.Wide_Wide.Text_IO.Integer_IO
--  and Ada.Wide_Wide_Text_IO.Modular_IO. The routines in this package are
--  identical semantically to those in Integer_IO and Modular_IO themselves,
--  except that the default parameters have been removed because they are
--  supplied explicitly by the calls from within these units.

private generic
   type Num is (<>);

   with function Scan
     (Str : String; Ptr : not null access Integer; Max : Integer) return Num;
   with procedure Set_Image
     (V : Num; S : in out String; P : in out Natural);
   with procedure Set_Image_Width
     (V : Num; W : Integer; S : out String; P : in out Natural);
   with procedure Set_Image_Based
     (V : Num; B : Natural; W : Integer; S : out String; P : in out Natural);

package Ada.Wide_Wide_Text_IO.Integer_Aux is

   procedure Get
     (File  : File_Type;
      Item  : out Num;
      Width : Field);

   procedure Gets
     (From : String;
      Item : out Num;
      Last : out Positive);

   procedure Put
     (File  : File_Type;
      Item  : Num;
      Width : Field;
      Base  : Number_Base);

   procedure Puts
     (To   : out String;
      Item : Num;
      Base : Number_Base);

end Ada.Wide_Wide_Text_IO.Integer_Aux;
