------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . W I D E _ T E X T _ I O . I N T E G E R _ A U X          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

--  This package contains the routines for Ada.Wide_Text_IO.Integer_IO that
--  are shared among separate instantiations of this package. The routines
--  in this package are identical semantically to those in Integer_IO itself,
--  except that the generic parameter Num has been replaced by Integer or
--  Long_Long_Integer, and the default parameters have been removed because
--  they are supplied explicitly by the calls from within the generic template.

private package Ada.Wide_Text_IO.Integer_Aux is

   procedure Get_Int
     (File  : File_Type;
      Item  : out Integer;
      Width : Field);

   procedure Get_LLI
     (File  : File_Type;
      Item  : out Long_Long_Integer;
      Width : Field);

   procedure Gets_Int
     (From : String;
      Item : out Integer;
      Last : out Positive);

   procedure Gets_LLI
     (From : String;
      Item : out Long_Long_Integer;
      Last : out Positive);

   procedure Put_Int
     (File  : File_Type;
      Item  : Integer;
      Width : Field;
      Base  : Number_Base);

   procedure Put_LLI
     (File  : File_Type;
      Item  : Long_Long_Integer;
      Width : Field;
      Base  : Number_Base);

   procedure Puts_Int
     (To   : out String;
      Item : Integer;
      Base : Number_Base);

   procedure Puts_LLI
     (To   : out String;
      Item : Long_Long_Integer;
      Base : Number_Base);

end Ada.Wide_Text_IO.Integer_Aux;
