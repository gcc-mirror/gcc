------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . W I D E _ T E X T _ I O . D E C I M A L _ A U X          --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001 Free Software Foundation, Inc.          --
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

--  This package contains the routines for Ada.Wide_Text_IO.Decimal_IO
--  that are shared among separate instantiations of this package. The
--  routines in the package are identical semantically to those declared
--  in Wide_Text_IO, except that default values have been supplied by the
--  generic, and the Num parameter has been replaced by Integer or
--  Long_Long_Integer, with an additional Scale parameter giving the
--  value of Num'Scale. In addition the Get routines return the value
--  rather than store it in an Out parameter.

private package Ada.Wide_Text_IO.Decimal_Aux is

   function Get_Dec
     (File  : File_Type;
      Width : Field;
      Scale : Integer)
      return  Integer;

   function Get_LLD
     (File  : File_Type;
      Width : Field;
      Scale : Integer)
      return  Long_Long_Integer;

   function Gets_Dec
     (From  : String;
      Last  : access Positive;
      Scale : Integer)
      return  Integer;

   function Gets_LLD
     (From  : String;
      Last  : access Positive;
      Scale : Integer)
      return  Long_Long_Integer;

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

end Ada.Wide_Text_IO.Decimal_Aux;
