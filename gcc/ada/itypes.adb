------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              I T Y P E S                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Einfo.Utils;    use Einfo.Utils;
with Sem;            use Sem;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Stand;          use Stand;
with Targparm;       use Targparm;

package body Itypes is

   ------------------
   -- Create_Itype --
   ------------------

   function Create_Itype
     (Ekind        : Entity_Kind;
      Related_Nod  : Node_Id;
      Related_Id   : Entity_Id := Empty;
      Suffix       : Character := ' ';
      Suffix_Index : Int       := 0;
      Scope_Id     : Entity_Id := Current_Scope) return Entity_Id
   is
      Typ : Entity_Id;

   begin
      --  Should comment setting of Public_Status here ???

      if Related_Id = Empty then
         Typ := New_Internal_Entity (Ekind, Scope_Id, Sloc (Related_Nod), 'T');
         Set_Public_Status (Typ);

      else
         Typ :=
           New_External_Entity
             (Ekind, Scope_Id, Sloc (Related_Nod), Related_Id, Suffix,
              Suffix_Index, 'T');
      end if;

      --  Make sure Esize (Typ) was properly initialized, it should be since
      --  New_Internal_Entity/New_External_Entity call Reinit_Size_Align.

      pragma Assert (not Known_Esize (Typ));

      Set_Etype (Typ, Any_Type);
      Set_Is_Itype (Typ);
      Set_Associated_Node_For_Itype (Typ, Related_Nod);

      if In_Deleted_Code then
         Set_Is_Frozen (Typ);
      end if;

      if Ekind in Access_Subprogram_Kind then
         Set_Can_Use_Internal_Rep (Typ, not Always_Compatible_Rep_On_Target);
      end if;

      return Typ;
   end Create_Itype;

   ---------------------------------
   -- Create_Null_Excluding_Itype --
   ---------------------------------

   function Create_Null_Excluding_Itype
      (T           : Entity_Id;
       Related_Nod : Node_Id;
       Scope_Id    : Entity_Id := Current_Scope) return Entity_Id
   is
      I_Typ : Entity_Id;

   begin
      pragma Assert (Is_Access_Type (T));

      I_Typ := Create_Itype (Ekind       => E_Access_Subtype,
                             Related_Nod => Related_Nod,
                             Scope_Id    => Scope_Id);

      Set_Directly_Designated_Type (I_Typ, Directly_Designated_Type (T));
      Set_Etype                    (I_Typ, Base_Type (T));
      Set_Depends_On_Private       (I_Typ, Depends_On_Private (T));
      Set_Is_Public                (I_Typ, Is_Public          (T));
      Set_From_Limited_With        (I_Typ, From_Limited_With  (T));
      Set_Is_Access_Constant       (I_Typ, Is_Access_Constant (T));
      Set_Is_Generic_Type          (I_Typ, Is_Generic_Type    (T));
      Set_Is_Volatile              (I_Typ, Is_Volatile        (T));
      Set_Treat_As_Volatile        (I_Typ, Treat_As_Volatile  (T));
      Set_Is_Atomic                (I_Typ, Is_Atomic          (T));
      Set_Is_Ada_2005_Only         (I_Typ, Is_Ada_2005_Only   (T));
      Set_Is_Ada_2012_Only         (I_Typ, Is_Ada_2012_Only   (T));
      Set_Is_Ada_2022_Only         (I_Typ, Is_Ada_2022_Only   (T));
      Set_Can_Never_Be_Null        (I_Typ);

      return I_Typ;
   end Create_Null_Excluding_Itype;

end Itypes;
