------------------------------------------------------------------------------
--                                                                          --
--                         GNAT LIBRARY COMPONENTS                          --
--                                                                          --
--            G N A T . M O S T _ R E C E N T _ E X C E P T I O N           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2000 Ada Core Technologies, Inc.              --
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

with Ada.Exceptions.Is_Null_Occurrence;
with System.Soft_Links;

package body GNAT.Most_Recent_Exception is

   ----------------
   -- Occurrence --
   ----------------

   function Occurrence
     return Ada.Exceptions.Exception_Occurrence
   is
      EOA : constant Ada.Exceptions.Exception_Occurrence_Access :=
              GNAT.Most_Recent_Exception.Occurrence_Access;

      use type Ada.Exceptions.Exception_Occurrence_Access;

   begin
      if EOA = null then
         return Ada.Exceptions.Null_Occurrence;
      else
         return EOA.all;
      end if;
   end Occurrence;

   -----------------------
   -- Occurrence_Access --
   -----------------------

   function Occurrence_Access
     return Ada.Exceptions.Exception_Occurrence_Access
   is
      use Ada.Exceptions;

      EOA : constant Exception_Occurrence_Access :=
              System.Soft_Links.Get_Current_Excep.all;

   begin
      if EOA = null then
         return null;

      elsif Is_Null_Occurrence (EOA.all) then
         return null;

      else
         return EOA;
      end if;
   end Occurrence_Access;

end GNAT.Most_Recent_Exception;
