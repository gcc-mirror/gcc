------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ T S S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;

package body Exp_Tss is

   --------------------
   -- Base_Init_Proc --
   --------------------

   function Base_Init_Proc (Typ : Entity_Id) return Entity_Id is
      Full_Type : E;
      Proc      : Entity_Id;

   begin
      pragma Assert (Ekind (Typ) in Type_Kind);

      if Is_Private_Type (Typ) then
         Full_Type := Underlying_Type (Base_Type (Typ));
      else
         Full_Type := Typ;
      end if;

      if No (Full_Type) then
         return Empty;
      elsif Is_Concurrent_Type (Full_Type)
        and then Present (Corresponding_Record_Type (Base_Type (Full_Type)))
      then
         return Init_Proc (Corresponding_Record_Type (Base_Type (Full_Type)));

      else
         Proc := Init_Proc (Base_Type (Full_Type));

         if No (Proc)
           and then Is_Composite_Type (Full_Type)
           and then Is_Derived_Type (Full_Type)
         then
            return Init_Proc (Root_Type (Full_Type));
         else
            return Proc;
         end if;
      end if;
   end Base_Init_Proc;

   --------------
   -- Copy_TSS --
   --------------

   --  Note: internally this routine is also used to initially set up
   --  a TSS entry for a new type (case of being called from Set_TSS)

   procedure Copy_TSS (TSS : Entity_Id; Typ : Entity_Id) is
      FN : Node_Id;

   begin
      Ensure_Freeze_Node (Typ);
      FN := Freeze_Node (Typ);

      if No (TSS_Elist (FN)) then
         Set_TSS_Elist (FN, New_Elmt_List);
      end if;

      --  We prepend here, so that a second call overrides the first, it
      --  is not clear that this is required, but it seems reasonable.

      Prepend_Elmt (TSS, TSS_Elist (FN));
   end Copy_TSS;

   ---------------------------------
   -- Has_Non_Null_Base_Init_Proc --
   ---------------------------------

   function Has_Non_Null_Base_Init_Proc (Typ : Entity_Id) return Boolean is
      BIP : constant Entity_Id := Base_Init_Proc (Typ);

   begin
      return Present (BIP) and then not Is_Null_Init_Proc (BIP);
   end Has_Non_Null_Base_Init_Proc;

   ---------------
   -- Init_Proc --
   ---------------

   function Init_Proc (Typ : Entity_Id) return Entity_Id is
   begin
      return TSS (Typ, Name_uInit_Proc);
   end Init_Proc;

   -------------------
   -- Set_Init_Proc --
   -------------------

   procedure Set_Init_Proc (Typ : Entity_Id; Init : Entity_Id) is
   begin
      Set_TSS (Typ, Init);
   end Set_Init_Proc;

   -------------
   -- Set_TSS --
   -------------

   procedure Set_TSS (Typ : Entity_Id; TSS : Entity_Id) is
      Subprog_Body : constant Node_Id := Unit_Declaration_Node (TSS);

   begin
      --  Case of insertion location is in unit defining the type

      if In_Same_Code_Unit (Typ, TSS) then
         Append_Freeze_Action (Typ, Subprog_Body);

      --  Otherwise, we are using an already existing TSS in another unit

      else
         null;
      end if;

      Copy_TSS (TSS, Typ);
   end Set_TSS;

   ---------
   -- TSS --
   ---------

   function TSS (Typ : Entity_Id; Nam : Name_Id) return Entity_Id is
      FN   : constant Node_Id := Freeze_Node (Typ);
      Elmt : Elmt_Id;
      Subp : Entity_Id;

   begin
      if No (FN) then
         return Empty;

      elsif No (TSS_Elist (FN)) then
         return Empty;

      else
         Elmt := First_Elmt (TSS_Elist (FN));

         while Present (Elmt) loop
            if Chars (Node (Elmt)) = Nam then
               Subp := Node (Elmt);

               --  For stream subprograms, the TSS entity may be a renaming-
               --  as-body of an already generated entity. Use that one rather
               --  the one introduced by the renaming, which is an artifact of
               --  current stream handling.

               if Nkind (Parent (Parent (Subp))) =
                                           N_Subprogram_Renaming_Declaration
                 and then
                   Present (Corresponding_Spec (Parent (Parent (Subp))))
               then
                  return Corresponding_Spec (Parent (Parent (Subp)));
               else
                  return Subp;
               end if;

            else
               Next_Elmt (Elmt);
            end if;
         end loop;
      end if;

      return Empty;
   end TSS;

end Exp_Tss;
