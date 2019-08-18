------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ T S S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Exp_Util; use Exp_Util;
with Nlists;   use Nlists;
with Lib;      use Lib;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;

package body Exp_Tss is

   --------------------
   -- Base_Init_Proc --
   --------------------

   function Base_Init_Proc
     (Typ : Entity_Id;
      Ref : Entity_Id := Empty) return Entity_Id
   is
      Full_Type : E;
      Proc      : Entity_Id;

   begin
      pragma Assert (Is_Type (Typ));

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
         --  The initialization routine to be called is that of the base type
         --  of the corresponding record type, which may itself be a subtype
         --  and possibly an itype.

         return Init_Proc
           (Base_Type (Corresponding_Record_Type (Base_Type (Full_Type))),
            Ref);

      else
         Proc := Init_Proc (Base_Type (Full_Type), Ref);

         if No (Proc)
           and then Is_Composite_Type (Full_Type)
           and then Is_Derived_Type (Full_Type)
         then
            return Init_Proc (Root_Type (Full_Type), Ref);
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

   -------------------
   -- CPP_Init_Proc --
   -------------------

   function CPP_Init_Proc (Typ  : Entity_Id) return Entity_Id is
      FN   : constant Node_Id := Freeze_Node (Typ);
      Elmt : Elmt_Id;

   begin
      if not Is_CPP_Class (Root_Type (Typ))
        or else No (FN)
        or else No (TSS_Elist (FN))
      then
         return Empty;

      else
         Elmt := First_Elmt (TSS_Elist (FN));
         while Present (Elmt) loop
            if Is_CPP_Init_Proc (Node (Elmt)) then
               return Node (Elmt);
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      return Empty;
   end CPP_Init_Proc;

   ------------------------
   -- Find_Inherited_TSS --
   ------------------------

   function Find_Inherited_TSS
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Entity_Id
   is
      Btyp : Entity_Id := Typ;
      Proc : Entity_Id;

   begin
      loop
         Btyp := Base_Type (Btyp);
         Proc := TSS (Btyp, Nam);

         exit when Present (Proc)
           or else not Is_Derived_Type (Btyp);

         --  If Typ is a derived type, it may inherit attributes from some
         --  ancestor.

         Btyp := Etype (Btyp);
      end loop;

      if No (Proc) then

         --  If nothing else, use the TSS of the root type

         Proc := TSS (Base_Type (Underlying_Type (Typ)), Nam);
      end if;

      return Proc;
   end Find_Inherited_TSS;

   ------------------
   -- Get_TSS_Name --
   ------------------

   function Get_TSS_Name (E : Entity_Id) return TSS_Name_Type is
      C1 : Character;
      C2 : Character;
      Nm : TSS_Name_Type;

   begin
      Get_Last_Two_Chars (Chars (E), C1, C2);

      if C1 in 'A' .. 'Z' and then C2 in 'A' .. 'Z' then
         Nm := (C1, C2);

         for J in TSS_Names'Range loop
            if Nm = TSS_Names (J) then
               return Nm;
            end if;
         end loop;
      end if;

      return TSS_Null;
   end Get_TSS_Name;

   ---------------------------------
   -- Has_Non_Null_Base_Init_Proc --
   ---------------------------------

   --  Note: if a base Init_Proc is present, and No_Default_Initialization is
   --  present, then we must avoid testing for a null init proc, since there
   --  is no init proc present in this case.

   function Has_Non_Null_Base_Init_Proc (Typ : Entity_Id) return Boolean is
      BIP : constant Entity_Id := Base_Init_Proc (Typ);
   begin
      return Present (BIP)
        and then (Restriction_Active (No_Default_Initialization)
                   or else not Is_Null_Init_Proc (BIP));
   end Has_Non_Null_Base_Init_Proc;

   ---------------
   -- Init_Proc --
   ---------------

   function Init_Proc
     (Typ  : Entity_Id;
      Ref  : Entity_Id := Empty) return Entity_Id
   is
      FN   : constant Node_Id := Freeze_Node (Typ);
      Elmt : Elmt_Id;
      E1   : Entity_Id;
      E2   : Entity_Id;

   begin
      if No (FN) then
         return Empty;

      elsif No (TSS_Elist (FN)) then
         return Empty;

      elsif No (Ref) then
         Elmt := First_Elmt (TSS_Elist (FN));
         while Present (Elmt) loop
            if Is_Init_Proc (Node (Elmt)) then
               if not Is_CPP_Class (Typ) then
                  return Node (Elmt);

               --  For CPP classes, we are looking for the default constructor,
               --  and so we must skip any non-default constructor.

               elsif
                 No (Next
                      (First
                        (Parameter_Specifications (Parent (Node (Elmt))))))
               then
                  return Node (Elmt);
               end if;
            end if;

            Next_Elmt (Elmt);
         end loop;

      --  Non-default constructors are currently supported only in the context
      --  of interfacing with C++.

      else pragma Assert (Is_CPP_Class (Typ));

         --  Use the referenced function to locate the init_proc matching
         --  the C++ constructor.

         Elmt := First_Elmt (TSS_Elist (FN));
         while Present (Elmt) loop
            if Is_Init_Proc (Node (Elmt)) then
               E1 := Next_Formal (First_Formal (Node (Elmt)));
               E2 := First_Formal (Ref);
               while Present (E1) and then Present (E2) loop
                  if Chars (E1) /= Chars (E2)
                    or else Ekind (E1) /= Ekind (E2)
                  then
                     exit;

                  elsif not Is_Anonymous_Access_Type (Etype (E1))
                    and then not Is_Anonymous_Access_Type (Etype (E2))
                    and then Etype (E1) /= Etype (E2)
                  then
                     exit;

                  elsif Ekind (Etype (E1)) = E_Anonymous_Access_Type
                    and then Ekind (Etype (E2)) = E_Anonymous_Access_Type
                    and then Directly_Designated_Type (Etype (E1))
                               /= Directly_Designated_Type (Etype (E2))
                  then
                     exit;

                  elsif Ekind_In (Etype (E1),
                          E_Anonymous_Access_Subprogram_Type,
                          E_Anonymous_Access_Protected_Subprogram_Type)
                    and then Ekind_In (Etype (E2),
                               E_Anonymous_Access_Subprogram_Type,
                               E_Anonymous_Access_Protected_Subprogram_Type)
                    and then not Conforming_Types
                                   (Etype (E1), Etype (E2), Fully_Conformant)
                  then
                     exit;
                  end if;

                  E1 := Next_Formal (E1);
                  E2 := Next_Formal (E2);
               end loop;

               if No (E1) and then No (E2) then
                  return Node (Elmt);
               end if;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      return Empty;
   end Init_Proc;

   ----------------------
   -- Is_CPP_Init_Proc --
   ----------------------

   function Is_CPP_Init_Proc (E : Entity_Id) return Boolean is
      C1 : Character;
      C2 : Character;
   begin
      Get_Last_Two_Chars (Chars (E), C1, C2);
      return C1 = TSS_CPP_Init_Proc (1) and then C2 = TSS_CPP_Init_Proc (2);
   end Is_CPP_Init_Proc;

   ------------------
   -- Is_Init_Proc --
   ------------------

   function Is_Init_Proc (E : Entity_Id) return Boolean is
      C1 : Character;
      C2 : Character;
   begin
      Get_Last_Two_Chars (Chars (E), C1, C2);
      return C1 = TSS_Init_Proc (1) and then C2 = TSS_Init_Proc (2);
   end Is_Init_Proc;

   ------------
   -- Is_TSS --
   ------------

   function Is_TSS (E : Entity_Id; Nam : TSS_Name_Type) return Boolean is
      C1 : Character;
      C2 : Character;
   begin
      Get_Last_Two_Chars (Chars (E), C1, C2);
      return C1 = Nam (1) and then C2 = Nam (2);
   end Is_TSS;

   function Is_TSS (N : Name_Id; Nam : TSS_Name_Type) return Boolean is
      C1 : Character;
      C2 : Character;
   begin
      Get_Last_Two_Chars (N, C1, C2);
      return C1 = Nam (1) and then C2 = Nam (2);
   end Is_TSS;

   -------------------------
   -- Make_Init_Proc_Name --
   -------------------------

   function Make_Init_Proc_Name (Typ : Entity_Id) return Name_Id is
   begin
      return Make_TSS_Name (Typ, TSS_Init_Proc);
   end Make_Init_Proc_Name;

   -------------------
   -- Make_TSS_Name --
   -------------------

   function Make_TSS_Name
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Name_Id
   is
   begin
      Get_Name_String (Chars (Typ));
      Add_Char_To_Name_Buffer (Nam (1));
      Add_Char_To_Name_Buffer (Nam (2));
      return Name_Find;
   end Make_TSS_Name;

   -------------------------
   -- Make_TSS_Name_Local --
   -------------------------

   function Make_TSS_Name_Local
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Name_Id
   is
   begin
      Get_Name_String (Chars (Typ));
      Add_Char_To_Name_Buffer ('_');
      Add_Nat_To_Name_Buffer (Increment_Serial_Number);
      Add_Char_To_Name_Buffer (Nam (1));
      Add_Char_To_Name_Buffer (Nam (2));
      return Name_Find;
   end Make_TSS_Name_Local;

   --------------
   -- Same_TSS --
   --------------

   function Same_TSS (E1, E2 : Entity_Id) return Boolean is
      E1C1 : Character;
      E1C2 : Character;
      E2C1 : Character;
      E2C2 : Character;

   begin
      Get_Last_Two_Chars (Chars (E1), E1C1, E1C2);
      Get_Last_Two_Chars (Chars (E2), E2C1, E2C2);

      return
        E1C1 = E2C1
          and then
        E1C2 = E2C2
          and then
        E1C1 in 'A' .. 'Z'
          and then
        E1C2 in 'A' .. 'Z';
   end Same_TSS;

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
   begin
      --  Make sure body of subprogram is frozen

      --  Skip this for Init_Proc with No_Default_Initialization, since the
      --  Init proc is a dummy void entity in this case to be ignored.

      if (Is_Init_Proc (TSS) or else Is_CPP_Init_Proc (TSS))
        and then Restriction_Active (No_Default_Initialization)
      then
         null;

      --  Skip this if not in the same code unit (since it means we are using
      --  an already existing TSS in another unit)

      elsif not In_Same_Code_Unit (Typ, TSS) then
         null;

      --  Otherwise make sure body is frozen

      else
         Append_Freeze_Action (Typ, Unit_Declaration_Node (TSS));
      end if;

      --  Set TSS entry

      Copy_TSS (TSS, Typ);
   end Set_TSS;

   ---------
   -- TSS --
   ---------

   function TSS (Typ : Entity_Id; Nam : TSS_Name_Type) return Entity_Id is
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
            if Is_TSS (Node (Elmt), Nam) then
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
