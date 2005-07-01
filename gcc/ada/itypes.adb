------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I T Y P E S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Einfo;    use Einfo;
with Opt;      use Opt;
with Sem;      use Sem;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Stand;    use Stand;

package body Itypes is

   ------------------
   -- Create_Itype --
   ------------------

   function Create_Itype
     (Ekind        : Entity_Kind;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id := Empty;
      Suffix       : Character := ' ';
      Suffix_Index : Nat       := 0;
      Scope_Id     : Entity_Id := Current_Scope)
      return         Entity_Id
   is
      Typ : Entity_Id;

   begin
      if Related_Id = Empty then
         Typ := New_Internal_Entity (Ekind, Scope_Id, Sloc (Related_Nod), 'T');
         Set_Public_Status (Typ);

      else
         Typ := New_External_Entity
           (Ekind, Scope_Id, Sloc (Related_Nod), Related_Id, Suffix,
               Suffix_Index, 'T');
      end if;

      Init_Size_Align (Typ);
      Set_Etype (Typ, Any_Type);
      Set_Is_Itype (Typ);
      Set_Associated_Node_For_Itype (Typ, Related_Nod);

      if In_Deleted_Code
        and then not ASIS_Mode
      then
         Set_Is_Frozen (Typ);
      end if;

      return Typ;
   end Create_Itype;

end Itypes;
