------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                S T R U B                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2021-2023, Free Software Foundation, Inc.       --
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

--  Package containing utility procedures related to Stack Scrubbing

with Atree;          use Atree;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Errout;         use Errout;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Sem_Eval;       use Sem_Eval;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stringt;        use Stringt;

package body Strub is
   -----------------------
   -- Local Subprograms --
   -----------------------

   function Find_Explicit_Strub_Pragma (Id : Entity_Id) return Node_Id;
   --  Return a pragma Machine_Attribute (Id, "strub"[, "mode"]) node
   --  if Id has one.

   function Strub_Pragma_Arg (Item : Node_Id) return Node_Id is
      (Get_Pragma_Arg
         (Next (Next (First (Pragma_Argument_Associations (Item))))));
   --  Return the pragma argument holding the strub mode associated
   --  with Item, a subprogram, variable, constant, or type. Bear in
   --  mind that strub pragmas with an explicit strub mode argument,
   --  naming access-to-subprogram types, are applied to the
   --  designated subprogram type.

   function Strub_Pragma_Arg_To_String (Item : Node_Id) return String is
      (To_String (Strval (Expr_Value_S (Item))));
   --  Extract and return as a String the strub mode held in a node
   --  returned by Strub_Pragma_Arg.

   function Strub_Pragma_Mode
     (Id   : Entity_Id;
      Item : Node_Id) return Strub_Mode;
   --  Return the strub mode associated with Item expressed in Id.
   --  Strub_Pragma_P (Id) must hold.

   ---------------------------
   -- Check_Same_Strub_Mode --
   ---------------------------

   procedure Check_Same_Strub_Mode
     (Dest, Src : Entity_Id;
      Report    : Boolean := True)
   is
      Src_Strub_Mode  : constant Strub_Mode := Explicit_Strub_Mode (Src);
      Dest_Strub_Mode : constant Strub_Mode := Explicit_Strub_Mode (Dest);

   begin
      if Dest_Strub_Mode = Src_Strub_Mode then
         return;
      end if;

      --  Internal is not part of the interface, it's an *internal*
      --  implementation detail, so consider it equivalent to unspecified here.
      --  ??? -fstrub=relaxed|strict makes them interface-equivalent to
      --  Callable or Disabled, respectively, but we don't look at that flag in
      --  the front-end, and it seems undesirable for that flag to affect
      --  whether specifications are conformant. Maybe there should be some
      --  means to specify Callable or Disabled along with Internal?

      if Dest_Strub_Mode in Unspecified | Internal
        and then Src_Strub_Mode in Unspecified | Internal
      then
         return;
      end if;

      if not Report then
         return;
      end if;

      if Src_Strub_Mode /= Unspecified then
         Error_Msg_Sloc := Sloc (Find_Explicit_Strub_Pragma (Src));
      else
         Error_Msg_Sloc := Sloc (Src);
      end if;
      Error_Msg_Node_2 := Src;
      Error_Msg_NE ("& requires the same `strub` mode as &#",
                    (if Dest_Strub_Mode /= Unspecified
                       then Find_Explicit_Strub_Pragma (Dest)
                       else Dest),
                    Dest);
   end Check_Same_Strub_Mode;

   ----------------------------
   -- Compatible_Strub_Modes --
   ----------------------------

   function Compatible_Strub_Modes
     (Dest, Src : Entity_Id) return Boolean
   is
      Src_Strub_Mode : constant Strub_Mode := Explicit_Strub_Mode (Src);
      Dest_Strub_Mode : constant Strub_Mode := Explicit_Strub_Mode (Dest);

   begin
      return Src_Strub_Mode = Dest_Strub_Mode
        or else At_Calls not in Src_Strub_Mode | Dest_Strub_Mode;
   end Compatible_Strub_Modes;

   ---------------------
   -- Copy_Strub_Mode --
   ---------------------

   procedure Copy_Strub_Mode (Dest, Src : Entity_Id) is
      Strub : Node_Id := Find_Explicit_Strub_Pragma (Src);
      Src_Strub_Mode : constant Strub_Mode := Strub_Pragma_Mode (Src, Strub);

   begin
      pragma Assert (Explicit_Strub_Mode (Dest) = Unspecified);

      --  Refrain from copying Internal to subprogram types.
      --  It affects code generation for the subprogram,
      --  but it has no effect on its type or interface.

      if Src_Strub_Mode = Unspecified
        or else (Ekind (Dest) = E_Subprogram_Type
                   and then Src_Strub_Mode = Internal)
      then
         return;
      end if;

      Strub := New_Copy (Strub);
      Set_Next_Rep_Item (Strub, First_Rep_Item (Dest));
      Set_First_Rep_Item (Dest, Strub);
      Set_Has_Gigi_Rep_Item (Dest);
   end Copy_Strub_Mode;

   -------------------------
   -- Explicit_Strub_Mode --
   -------------------------

   function Explicit_Strub_Mode (Id : Entity_Id) return Strub_Mode is
      Item : constant Node_Id := Find_Explicit_Strub_Pragma (Id);

   begin
      return Strub_Pragma_Mode (Id, Item);
   end Explicit_Strub_Mode;

   --------------------------------
   -- Find_Explicit_Strub_Pragma --
   --------------------------------

   function Find_Explicit_Strub_Pragma (Id : Entity_Id) return Node_Id is
      Item : Node_Id;

   begin
      if not Has_Gigi_Rep_Item (Id) then
         return Empty;
      end if;

      Item := First_Rep_Item (Id);
      while Present (Item) loop
         if Strub_Pragma_P (Item) then
            return Item;
         end if;
         Item := Next_Rep_Item (Item);
      end loop;

      return Empty;
   end Find_Explicit_Strub_Pragma;

   -----------------------
   -- Strub_Pragma_Mode --
   -----------------------

   function Strub_Pragma_Mode
     (Id   : Entity_Id;
      Item : Node_Id) return Strub_Mode
   is
      Arg : Node_Id := Empty;

   begin
      --  ??? Enumeration literals, despite being conceptually functions, have
      --  neither bodies nor stack frames, and it's not clear whether it would
      --  make more sense to treat them as subprograms or as constants, but
      --  they can be renamed as functions.  Should we require all literals of
      --  a type to have the same strub mode?  Rule out their annotation?

      if Ekind (Id) in E_Subprogram_Type
                     | Overloadable_Kind
                     | Generic_Subprogram_Kind
      then
         if Item = Empty then
            return Unspecified;
         end if;

         Arg := Strub_Pragma_Arg (Item);
         if Arg = Empty then
            return At_Calls;
         end if;

         declare
            Str : constant String := Strub_Pragma_Arg_To_String (Arg);
         begin
            if Str'Length /= 8 then
               return Unspecified;
            end if;

            case Str (Str'First) is
               when 'a' =>
                  if Str = "at-calls" then
                     return At_Calls;
                  end if;

               when 'i' =>
                  if Str = "internal" then
                     return Internal;
                  end if;

               when 'c' =>
                  if Str = "callable" then
                     return Callable;
                  end if;

               when 'd' =>
                  if Str = "disabled" then
                     return Disabled;
                  end if;

               when others =>
                  null;
            end case;
            return Unspecified;
         end;

      --  Access-to-subprogram types and variables can be treated just like
      --  other access types, because the pragma logic has already promoted to
      --  subprogram types any annotations applicable to them.

      elsif Ekind (Id) in Type_Kind -- except E_Subprogram_Type, covered above
                        | Scalar_Kind
                        | Object_Kind
                        | Named_Kind
      then
         if Item = Empty then
            return Unspecified;
         end if;

         Arg := Strub_Pragma_Arg (Item);
         if Arg /= Empty then
            --  A strub parameter is not applicable to variables,
            --  and will be ignored.

            return Unspecified;
         end if;

         return Enabled;

      else
         pragma Assert (Item = Empty);
         return Not_Applicable;
      end if;
   end Strub_Pragma_Mode;

   --------------------
   -- Strub_Pragma_P --
   --------------------

   function Strub_Pragma_P
     (Item : Node_Id) return Boolean is
      (Nkind (Item) = N_Pragma
         and then Pragma_Name (Item) = Name_Machine_Attribute
         and then
           Strub_Pragma_Arg_To_String
             (Get_Pragma_Arg
                (Next (First (Pragma_Argument_Associations (Item)))))
             = "strub");

end Strub;
