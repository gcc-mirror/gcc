------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . T E X T _ I O . I N T E G E R _ A U X               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
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

--  This package contains the routines for Ada.Text_IO.Integer_IO that are
--  shared among separate instantiations of this package. The routines in
--  this package are identical semantically to those in Integer_IO itself,
--  except that the generic parameter Num has been replaced by Integer or
--  Long_Long_Integer, and the default parameters have been removed because
--  they are supplied explicitly by the calls from within the generic template.

private package Ada.Text_IO.Integer_Aux is

   procedure Get_Int
     (File  : in File_Type;
      Item  : out Integer;
      Width : in Field);

   procedure Get_LLI
     (File  : in File_Type;
      Item  : out Long_Long_Integer;
      Width : in Field);

   procedure Put_Int
     (File  : in File_Type;
      Item  : in Integer;
      Width : in Field;
      Base  : in Number_Base);

   procedure Put_LLI
     (File  : in File_Type;
      Item  : in Long_Long_Integer;
      Width : in Field;
      Base  : in Number_Base);

   procedure Gets_Int
     (From : in String;
      Item : out Integer;
      Last : out Positive);

   procedure Gets_LLI
     (From : in String;
      Item : out Long_Long_Integer;
      Last : out Positive);

   procedure Puts_Int
     (To   : out String;
      Item : in Integer;
      Base : in Number_Base);

   procedure Puts_LLI
     (To   : out String;
      Item : in Long_Long_Integer;
      Base : in Number_Base);

end Ada.Text_IO.Integer_Aux;
