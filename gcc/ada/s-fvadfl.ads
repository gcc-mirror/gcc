------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . F A T _ V A X _ D _ F L O A T                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This package contains an instantiation of the floating-point attribute
--  runtime routines for VAX D-float for use on VMS targets.

with System.Fat_Gen;

package System.Fat_VAX_D_Float is
   pragma Pure;

   pragma Warnings (Off);
   --  This unit is normally used only for VMS, but we compile it for other
   --  targest for the convenience of testing vms code using -gnatdm.

   type Fat_VAX_D is digits 9;
   pragma Float_Representation (VAX_Float, Fat_VAX_D);

   --  Note the only entity from this package that is acccessed by Rtsfind
   --  is the name of the package instantiation. Entities within this package
   --  (i.e. the individual floating-point attribute routines) are accessed
   --  by name using selected notation.

   package Attr_VAX_D_Float is new System.Fat_Gen (Fat_VAX_D);

end System.Fat_VAX_D_Float;
