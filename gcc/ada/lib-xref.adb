------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . X R E F                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.6 $
--                                                                          --
--          Copyright (C) 1998-2001, Free Software Foundation, Inc.         --
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
with Csets;    use Csets;
with Debug;    use Debug;
with Lib.Util; use Lib.Util;
with Namet;    use Namet;
with Opt;      use Opt;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Table;    use Table;
with Widechar; use Widechar;

with GNAT.Heap_Sort_A;

package body Lib.Xref is

   ------------------
   -- Declarations --
   ------------------

   --  The Xref table is used to record references. The Loc field is set
   --  to No_Location for a definition entry.

   subtype Xref_Entry_Number is Int;

   type Xref_Entry is record
      Ent : Entity_Id;
      --  Entity referenced (E parameter to Generate_Reference)

      Def : Source_Ptr;
      --  Original source location for entity being referenced. Note that
      --  these values are used only during the output process, they are
      --  not set when the entries are originally built. This is because
      --  private entities can be swapped when the initial call is made.

      Loc : Source_Ptr;
      --  Location of reference (Original_Location (Sloc field of N parameter
      --  to Generate_Reference). Set to No_Location for the case of a
      --  defining occurrence.

      Typ : Character;
      --  Reference type (Typ param to Generate_Reference)

      Eun : Unit_Number_Type;
      --  Unit number corresponding to Ent

      Lun : Unit_Number_Type;
      --  Unit number corresponding to Loc. Value is undefined and not
      --  referenced if Loc is set to No_Location.

   end record;

   package Xrefs is new Table.Table (
     Table_Component_Type => Xref_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Xrefs_Initial,
     Table_Increment      => Alloc.Xrefs_Increment,
     Table_Name           => "Xrefs");

   -------------------------
   -- Generate_Definition --
   -------------------------

   procedure Generate_Definition (E : Entity_Id) is
      Loc  : Source_Ptr;
      Indx : Nat;

   begin
      pragma Assert (Nkind (E) in N_Entity);

      --  Note that we do not test Xref_Entity_Letters here. It is too
      --  early to do so, since we are often called before the entity
      --  is fully constructed, so that the Ekind is still E_Void.

      if Opt.Xref_Active

         --  Definition must come from source

         and then Comes_From_Source (E)

         --  And must have a reasonable source location that is not
         --  within an instance (all entities in instances are ignored)

         and then Sloc (E) > No_Location
         and then Instantiation_Location (Sloc (E)) = No_Location

         --  And must be a non-internal name from the main source unit

         and then In_Extended_Main_Source_Unit (E)
         and then not Is_Internal_Name (Chars (E))
      then
         Xrefs.Increment_Last;
         Indx := Xrefs.Last;
         Loc  := Original_Location (Sloc (E));

         Xrefs.Table (Indx).Ent := E;
         Xrefs.Table (Indx).Loc := No_Location;
         Xrefs.Table (Indx).Eun := Get_Source_Unit (Loc);
         Xrefs.Table (Indx).Lun := No_Unit;
      end if;
   end Generate_Definition;

   ---------------------------------
   -- Generate_Operator_Reference --
   ---------------------------------

   procedure Generate_Operator_Reference (N : Node_Id) is
   begin
      if not In_Extended_Main_Source_Unit (N) then
         return;
      end if;

      --  If the operator is not a Standard operator, then we generate
      --  a real reference to the user defined operator.

      if Sloc (Entity (N)) /= Standard_Location then
         Generate_Reference (Entity (N), N);

         --  A reference to an implicit inequality operator is a also a
         --  reference to the user-defined equality.

         if Nkind (N) = N_Op_Ne
           and then not Comes_From_Source (Entity (N))
           and then Present (Corresponding_Equality (Entity (N)))
         then
            Generate_Reference (Corresponding_Equality (Entity (N)), N);
         end if;

      --  For the case of Standard operators, we mark the result type
      --  as referenced. This ensures that in the case where we are
      --  using a derived operator, we mark an entity of the unit that
      --  implicitly defines this operator as used. Otherwise we may
      --  think that no entity of the unit is used. The actual entity
      --  marked as referenced is the first subtype, which is the user
      --  defined entity that is relevant.

      else
         if Nkind (N) = N_Op_Eq
           or else Nkind (N) = N_Op_Ne
           or else Nkind (N) = N_Op_Le
           or else Nkind (N) = N_Op_Lt
           or else Nkind (N) = N_Op_Ge
           or else Nkind (N) = N_Op_Gt
         then
            Set_Referenced (First_Subtype (Etype (Right_Opnd (N))));
         else
            Set_Referenced (First_Subtype (Etype (N)));
         end if;
      end if;
   end Generate_Operator_Reference;

   ------------------------
   -- Generate_Reference --
   ------------------------

   procedure Generate_Reference
     (E       : Entity_Id;
      N       : Node_Id;
      Typ     : Character := 'r';
      Set_Ref : Boolean   := True;
      Force   : Boolean   := False)
   is
      Indx : Nat;
      Nod  : Node_Id;
      Ref  : Source_Ptr;
      Def  : Source_Ptr;
      Ent  : Entity_Id;

   begin
      pragma Assert (Nkind (E) in N_Entity);

      --  Never collect references if not in main source unit. However,
      --  we omit this test if Typ is 'e', since these entries are
      --  really structural, and it is useful to have them in units
      --  that reference packages as well as units that define packages.

      if not In_Extended_Main_Source_Unit (N)
        and then Typ /= 'e'
      then
         return;
      end if;

      --  Unless the reference is forced, we ignore references where
      --  the reference itself does not come from Source.

      if not Force and then not Comes_From_Source (N) then
         return;
      end if;

      --  Deal with setting entity as referenced, unless suppressed.
      --  Note that we still do Set_Referenced on entities that do not
      --  come from source. This situation arises when we have a source
      --  reference to a derived operation, where the derived operation
      --  itself does not come from source, but we still want to mark it
      --  as referenced, since we really are referencing an entity in the
      --  corresponding package (this avoids incorrect complaints that the
      --  package contains no referenced entities).

      if Set_Ref then
         Set_Referenced (E);

         --  If this is a subprogram instance, mark as well the internal
         --  subprogram in the wrapper package, which may be a visible
         --  compilation unit.

         if Is_Overloadable (E)
           and then Is_Generic_Instance (E)
           and then Present (Alias (E))
         then
            Set_Referenced (Alias (E));
         end if;
      end if;

      --  Generate reference if all conditions are met:

      if
         --  Cross referencing must be active

         Opt.Xref_Active

         --  The entity must be one for which we collect references

         and then Xref_Entity_Letters (Ekind (E)) /= ' '

         --  Both Sloc values must be set to something sensible

         and then Sloc (E) > No_Location
         and then Sloc (N) > No_Location

         --  We ignore references from within an instance

         and then Instantiation_Location (Sloc (N)) = No_Location

         --  Ignore dummy references

        and then Typ /= ' '
      then
         if Nkind (N) = N_Identifier
              or else
            Nkind (N) = N_Defining_Identifier
              or else
            Nkind (N) in N_Op
              or else
            Nkind (N) = N_Defining_Operator_Symbol
              or else
            (Nkind (N) = N_Character_Literal
              and then Sloc (Entity (N)) /= Standard_Location)
              or else
            Nkind (N) = N_Defining_Character_Literal
         then
            Nod := N;

         elsif Nkind (N) = N_Expanded_Name
                 or else
               Nkind (N) = N_Selected_Component
         then
            Nod := Selector_Name (N);

         else
            return;
         end if;

         --  Normal case of source entity comes from source

         if Comes_From_Source (E) then
            Ent := E;

         --  Entity does not come from source, but is a derived subprogram
         --  and the derived subprogram comes from source, in which case
         --  the reference is to this parent subprogram.

         elsif Is_Overloadable (E)
           and then Present (Alias (E))
           and then Comes_From_Source (Alias (E))
         then
            Ent := Alias (E);

         --  Ignore reference to any other source that is not from source

         else
            return;
         end if;

         --  Record reference to entity

         Ref := Original_Location (Sloc (Nod));
         Def := Original_Location (Sloc (Ent));

         Xrefs.Increment_Last;
         Indx := Xrefs.Last;

         Xrefs.Table (Indx).Loc := Ref;
         Xrefs.Table (Indx).Typ := Typ;
         Xrefs.Table (Indx).Eun := Get_Source_Unit (Def);
         Xrefs.Table (Indx).Lun := Get_Source_Unit (Ref);
         Xrefs.Table (Indx).Ent := Ent;
      end if;
   end Generate_Reference;

   -----------------------
   -- Output_References --
   -----------------------

   procedure Output_References is
      Nrefs : constant Nat := Xrefs.Last;

      Rnums : array (0 .. Nrefs) of Nat;
      --  This array contains numbers of references in the Xrefs table. This
      --  list is sorted in output order. The extra 0'th entry is convenient
      --  for the call to sort. When we sort the table, we move these entries
      --  around, but we do not move the original table entries.

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Comparison function for Sort call

      procedure Move (From : Natural; To : Natural);
      --  Move procedure for Sort call

      function Lt (Op1, Op2 : Natural) return Boolean is
         T1 : Xref_Entry renames Xrefs.Table (Rnums (Nat (Op1)));
         T2 : Xref_Entry renames Xrefs.Table (Rnums (Nat (Op2)));

      begin
         --  First test. If entity is in different unit, sort by unit

         if T1.Eun /= T2.Eun then
            return Dependency_Num (T1.Eun) < Dependency_Num (T2.Eun);

         --  Second test, within same unit, sort by entity Sloc

         elsif T1.Def /= T2.Def then
            return T1.Def < T2.Def;

         --  Third test, sort definitions ahead of references

         elsif T1.Loc = No_Location then
            return True;

         elsif T2.Loc = No_Location then
            return False;

         --  Fourth test, for same entity, sort by reference location unit

         elsif T1.Lun /= T2.Lun then
            return Dependency_Num (T1.Lun) < Dependency_Num (T2.Lun);

         --  Fifth test order of location within referencing unit

         elsif T1.Loc /= T2.Loc then
            return T1.Loc < T2.Loc;

         --  Finally, for two locations at the same address, we prefer
         --  the one that does NOT have the type 'r' so that a modification
         --  or extension takes preference, when there are more than one
         --  reference at the same location.

         else
            return T2.Typ = 'r';
         end if;
      end Lt;

      procedure Move (From : Natural; To : Natural) is
      begin
         Rnums (Nat (To)) := Rnums (Nat (From));
      end Move;

   --  Start of processing for Output_References

   begin
      if not Opt.Xref_Active then
         return;
      end if;

      --  Capture the definition Sloc values. We delay doing this till now,
      --  since at the time the reference or definition is made, private
      --  types may be swapped, and the Sloc value may be incorrect. We
      --  also set up the pointer vector for the sort.

      for J in 1 .. Nrefs loop
         Rnums (J) := J;
         Xrefs.Table (J).Def :=
           Original_Location (Sloc (Xrefs.Table (J).Ent));
      end loop;

      --  Sort the references

      GNAT.Heap_Sort_A.Sort
        (Integer (Nrefs),
         Move'Unrestricted_Access,
         Lt'Unrestricted_Access);

      --  Now output the references

      Output_Refs : declare

         Curxu : Unit_Number_Type;
         --  Current xref unit

         Curru : Unit_Number_Type;
         --  Current reference unit for one entity

         Cursrc : Source_Buffer_Ptr;
         --  Current xref unit source text

         Curent : Entity_Id;
         --  Current entity

         Curnam : String (1 .. Name_Buffer'Length);
         Curlen : Natural;
         --  Simple name and length of current entity

         Curdef : Source_Ptr;
         --  Original source location for current entity

         Crloc : Source_Ptr;
         --  Current reference location

         Ctyp : Character;
         --  Entity type character

         Tref : Entity_Id;
         --  Type reference

         Rref : Node_Id;
         --  Renaming reference

         Trunit : Unit_Number_Type;
         --  Unit number for type reference

         function Name_Change (X : Entity_Id) return Boolean;
         --  Determines if entity X has a different simple name from Curent

         -----------------
         -- Name_Change --
         -----------------

         function Name_Change (X : Entity_Id) return Boolean is
         begin
            Get_Unqualified_Name_String (Chars (X));

            if Name_Len /= Curlen then
               return True;

            else
               return Name_Buffer (1 .. Curlen) /= Curnam (1 .. Curlen);
            end if;
         end Name_Change;

      --  Start of processing for Output_Refs

      begin
         Curxu  := No_Unit;
         Curent := Empty;
         Curdef := No_Location;
         Curru  := No_Unit;
         Crloc  := No_Location;

         for Refno in 1 .. Nrefs loop

            Output_One_Ref : declare

               XE : Xref_Entry renames Xrefs.Table (Rnums (Refno));
               --  The current entry to be accessed

               P : Source_Ptr;
               --  Used to index into source buffer to get entity name

               P2  : Source_Ptr;
               WC  : Char_Code;
               Err : Boolean;
               Ent : Entity_Id;
               Sav : Entity_Id;

               Left  : Character;
               Right : Character;
               --  Used for {} or <> for type reference

               procedure Output_Instantiation_Refs (Loc : Source_Ptr);
               --  Recursive procedure to output instantiation references for
               --  the given source ptr in [file|line[...]] form. No output
               --  if the given location is not a generic template reference.

               -------------------------------
               -- Output_Instantiation_Refs --
               -------------------------------

               procedure Output_Instantiation_Refs (Loc : Source_Ptr) is
                  Iloc : constant Source_Ptr := Instantiation_Location (Loc);
                  Lun  : Unit_Number_Type;

               begin
                  --  Nothing to do if this is not an instantiation

                  if Iloc = No_Location then
                     return;
                  end if;

                  --  For now, nothing to do unless special debug flag set

                  if not Debug_Flag_MM then
                     return;
                  end if;

                  --  Output instantiation reference

                  Write_Info_Char ('[');
                  Lun := Get_Source_Unit (Iloc);

                  if Lun /= Curru then
                     Curru := XE.Lun;
                     Write_Info_Nat (Dependency_Num (Curru));
                     Write_Info_Char ('|');
                  end if;

                  Write_Info_Nat (Int (Get_Logical_Line_Number (Iloc)));

                  --  Recursive call to get nested instantiations

                  Output_Instantiation_Refs (Iloc);

                  --  Output final ] after call to get proper nesting

                  Write_Info_Char (']');
                  return;
               end Output_Instantiation_Refs;

            --  Start of processing for Output_One_Ref

            begin
               Ent := XE.Ent;
               Ctyp := Xref_Entity_Letters (Ekind (Ent));

               --  Skip reference if it is the only reference to an entity,
               --  and it is an end-line reference, and the entity is not in
               --  the current extended source. This prevents junk entries
               --  consisting only of packages with end lines, where no
               --  entity from the package is actually referenced.

               if XE.Typ = 'e'
                 and then Ent /= Curent
                 and then (Refno = Nrefs or else
                             Ent /= Xrefs.Table (Rnums (Refno + 1)).Ent)
                 and then
                   not In_Extended_Main_Source_Unit (Ent)
               then
                  goto Continue;
               end if;

               --  For private type, get full view type

               if Ctyp = '+'
                 and then Present (Full_View (XE.Ent))
               then
                  Ent := Underlying_Type (Ent);

                  if Present (Ent) then
                     Ctyp := Xref_Entity_Letters (Ekind (Ent));
                  end if;
               end if;

               --  Special exception for Boolean

               if Ctyp = 'E' and then Is_Boolean_Type (Ent) then
                  Ctyp := 'B';
               end if;

               --  For variable reference, get corresponding type

               if Ctyp = '*' then
                  Ent := Etype (XE.Ent);
                  Ctyp := Fold_Lower (Xref_Entity_Letters (Ekind (Ent)));

                  --  If variable is private type, get full view type

                  if Ctyp = '+'
                    and then Present (Full_View (Etype (XE.Ent)))
                  then
                     Ent := Underlying_Type (Etype (XE.Ent));

                     if Present (Ent) then
                        Ctyp := Xref_Entity_Letters (Ekind (Ent));
                     end if;
                  end if;

                  --  Special handling for access parameter

                  if Ekind (Etype (XE.Ent)) = E_Anonymous_Access_Type
                    and then Is_Formal (XE.Ent)
                  then
                     Ctyp := 'p';

                  --  Special handling for Boolean

                  elsif Ctyp = 'e' and then Is_Boolean_Type (Ent) then
                     Ctyp := 'b';
                  end if;
               end if;

               --  Only output reference if interesting type of entity,
               --  and suppress self references. Also suppress definitions
               --  of body formals (we only treat these as references, and
               --  the references were separately recorded).

               if Ctyp /= ' '
                 and then XE.Loc /= XE.Def
                 and then (not Is_Formal (XE.Ent)
                            or else No (Spec_Entity (XE.Ent)))
               then
                  --  Start new Xref section if new xref unit

                  if XE.Eun /= Curxu then

                     if Write_Info_Col > 1 then
                        Write_Info_EOL;
                     end if;

                     Curxu := XE.Eun;
                     Cursrc := Source_Text (Source_Index (Curxu));

                     Write_Info_Initiate ('X');
                     Write_Info_Char (' ');
                     Write_Info_Nat (Dependency_Num (XE.Eun));
                     Write_Info_Char (' ');
                     Write_Info_Name (Reference_Name (Source_Index (XE.Eun)));
                  end if;

                  --  Start new Entity line if new entity. Note that we
                  --  consider two entities the same if they have the same
                  --  name and source location. This causes entities in
                  --  instantiations to be treated as though they referred
                  --  to the template.

                  if No (Curent)
                    or else
                      (XE.Ent /= Curent
                         and then
                           (Name_Change (XE.Ent) or else XE.Def /= Curdef))
                  then
                     Curent := XE.Ent;
                     Curdef := XE.Def;

                     Get_Unqualified_Name_String (Chars (XE.Ent));
                     Curlen := Name_Len;
                     Curnam (1 .. Curlen) := Name_Buffer (1 .. Curlen);

                     if Write_Info_Col > 1 then
                        Write_Info_EOL;
                     end if;

                     --  Write column number information

                     Write_Info_Nat (Int (Get_Logical_Line_Number (XE.Def)));
                     Write_Info_Char (Ctyp);
                     Write_Info_Nat (Int (Get_Column_Number (XE.Def)));

                     --  Write level information

                     if Is_Public (Curent) and then not Is_Hidden (Curent) then
                        Write_Info_Char ('*');
                     else
                        Write_Info_Char (' ');
                     end if;

                     --  Output entity name. We use the occurrence from the
                     --  actual source program at the definition point

                     P := Original_Location (Sloc (XE.Ent));

                     --  Entity is character literal

                     if Cursrc (P) = ''' then
                        Write_Info_Char (Cursrc (P));
                        Write_Info_Char (Cursrc (P + 1));
                        Write_Info_Char (Cursrc (P + 2));

                     --  Entity is operator symbol

                     elsif Cursrc (P) = '"' or else Cursrc (P) = '%' then
                        Write_Info_Char (Cursrc (P));

                        P2 := P;
                        loop
                           P2 := P2 + 1;
                           Write_Info_Char (Cursrc (P2));
                           exit when Cursrc (P2) = Cursrc (P);
                        end loop;

                     --  Entity is identifier

                     else
                        loop
                           if Is_Start_Of_Wide_Char (Cursrc, P) then
                              Scan_Wide (Cursrc, P, WC, Err);
                           elsif not Identifier_Char (Cursrc (P)) then
                              exit;
                           else
                              P := P + 1;
                           end if;
                        end loop;

                        for J in
                          Original_Location (Sloc (XE.Ent)) .. P - 1
                        loop
                           Write_Info_Char (Cursrc (J));
                        end loop;
                     end if;

                     --  See if we have a renaming reference

                     if Is_Object (XE.Ent)
                       and then Present (Renamed_Object (XE.Ent))
                     then
                        Rref := Renamed_Object (XE.Ent);

                     elsif Is_Overloadable (XE.Ent)
                       and then Nkind (Parent (Declaration_Node (XE.Ent))) =
                                            N_Subprogram_Renaming_Declaration
                     then
                        Rref := Name (Parent (Declaration_Node (XE.Ent)));

                     elsif Ekind (XE.Ent) = E_Package
                       and then Nkind (Declaration_Node (XE.Ent)) =
                                         N_Package_Renaming_Declaration
                     then
                        Rref := Name (Declaration_Node (XE.Ent));

                     else
                        Rref := Empty;
                     end if;

                     if Present (Rref) then
                        if Nkind (Rref) = N_Expanded_Name then
                           Rref := Selector_Name (Rref);
                        end if;

                        if Nkind (Rref) /= N_Identifier then
                           Rref := Empty;
                        end if;
                     end if;

                     --  Write out renaming reference if we have one

                     if Debug_Flag_MM and then Present (Rref) then
                        Write_Info_Char ('=');
                        Write_Info_Nat
                          (Int (Get_Logical_Line_Number (Sloc (Rref))));
                        Write_Info_Char (':');
                        Write_Info_Nat
                          (Int (Get_Column_Number (Sloc (Rref))));
                     end if;

                     --  See if we have a type reference

                     Tref := XE.Ent;
                     Left := '{';
                     Right := '}';

                     loop
                        Sav := Tref;

                        --  Processing for types

                        if Is_Type (Tref) then

                           --  Case of base type

                           if Base_Type (Tref) = Tref then

                              --  If derived, then get first subtype

                              if Tref /= Etype (Tref) then
                                 Tref := First_Subtype (Etype (Tref));

                                 --  Set brackets for derived type, but don't
                                 --  override pointer case since the fact that
                                 --  something is a pointer is more important

                                 if Left /= '(' then
                                    Left := '<';
                                    Right := '>';
                                 end if;

                              --  If non-derived ptr, get designated type

                              elsif Is_Access_Type (Tref) then
                                 Tref := Designated_Type (Tref);
                                 Left := '(';
                                 Right := ')';

                              --  For other non-derived base types, nothing

                              else
                                 exit;
                              end if;

                           --  For a subtype, go to ancestor subtype

                           else
                              Tref := Ancestor_Subtype (Tref);

                              --  If no ancestor subtype, go to base type

                              if No (Tref) then
                                 Tref := Base_Type (Sav);
                              end if;
                           end if;

                        --  For objects, functions, enum literals,
                        --  just get type from Etype field.

                        elsif Is_Object (Tref)
                          or else Ekind (Tref) = E_Enumeration_Literal
                          or else Ekind (Tref) = E_Function
                          or else Ekind (Tref) = E_Operator
                        then
                           Tref := Etype (Tref);

                        --  For anything else, exit

                        else
                           exit;
                        end if;

                        --  Exit if no type reference, or we are stuck in
                        --  some loop trying to find the type reference.

                        exit when No (Tref) or else Tref = Sav;

                        --  Here we have a type reference to output

                        --  Case of standard entity, output name

                        if Sloc (Tref) = Standard_Location then

                           --  For now, output only if special -gnatdM flag set

                           exit when not Debug_Flag_MM;

                           Write_Info_Char (Left);
                           Write_Info_Name (Chars (Tref));
                           Write_Info_Char (Right);
                           exit;

                        --  Case of source entity, output location

                        elsif Comes_From_Source (Tref) then

                           --  For now, output only derived type entries
                           --  unless we have special debug flag -gnatdM

                           exit when not (Debug_Flag_MM or else Left = '<');

                           --  Do not output type reference if referenced
                           --  entity is not in the main unit and is itself
                           --  not referenced, since otherwise the reference
                           --  will dangle.

                           exit when not Referenced (Tref)
                             and then not In_Extended_Main_Source_Unit (Tref);

                           --  Output the reference

                           Write_Info_Char (Left);
                           Trunit := Get_Source_Unit (Sloc (Tref));

                           if Trunit /= Curxu then
                              Write_Info_Nat (Dependency_Num (Trunit));
                              Write_Info_Char ('|');
                           end if;

                           Write_Info_Nat
                             (Int (Get_Logical_Line_Number (Sloc (Tref))));
                           Write_Info_Char
                             (Xref_Entity_Letters (Ekind (Tref)));
                           Write_Info_Nat
                             (Int (Get_Column_Number (Sloc (Tref))));
                           Write_Info_Char (Right);
                           exit;

                        --  If non-standard, non-source entity, keep looking

                        else
                           null;
                        end if;
                     end loop;

                     --  End of processing for entity output

                     Curru := Curxu;
                     Crloc := No_Location;
                  end if;

                  --  Output the reference

                  if XE.Loc /= No_Location
                     and then XE.Loc /= Crloc
                  then
                     Crloc := XE.Loc;

                     --  Start continuation if line full, else blank

                     if Write_Info_Col > 72 then
                        Write_Info_EOL;
                        Write_Info_Initiate ('.');
                     end if;

                     Write_Info_Char (' ');

                     --  Output file number if changed

                     if XE.Lun /= Curru then
                        Curru := XE.Lun;
                        Write_Info_Nat (Dependency_Num (Curru));
                        Write_Info_Char ('|');
                     end if;

                     Write_Info_Nat  (Int (Get_Logical_Line_Number (XE.Loc)));
                     Write_Info_Char (XE.Typ);
                     Write_Info_Nat  (Int (Get_Column_Number (XE.Loc)));

                     Output_Instantiation_Refs (Sloc (XE.Ent));
                  end if;
               end if;
            end Output_One_Ref;

         <<Continue>>
            null;
         end loop;

         Write_Info_EOL;
      end Output_Refs;
   end Output_References;

end Lib.Xref;
