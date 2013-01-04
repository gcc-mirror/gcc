------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ M E C H                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2012, Free Software Foundation, Inc.         --
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
with Errout;   use Errout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Targparm; use Targparm;

package body Sem_Mech is

   -------------------------
   -- Set_Mechanism_Value --
   -------------------------

   procedure Set_Mechanism_Value (Ent : Entity_Id; Mech_Name : Node_Id) is
      Class : Node_Id;
      Param : Node_Id;

      procedure Bad_Class;
      --  Signal bad descriptor class name

      procedure Bad_Mechanism;
      --  Signal bad mechanism name

      procedure Bad_Class is
      begin
         Error_Msg_N ("unrecognized descriptor class name", Class);
      end Bad_Class;

      procedure Bad_Mechanism is
      begin
         Error_Msg_N ("unrecognized mechanism name", Mech_Name);
      end Bad_Mechanism;

   --  Start of processing for Set_Mechanism_Value

   begin
      if Mechanism (Ent) /= Default_Mechanism then
         Error_Msg_NE
           ("mechanism for & has already been set", Mech_Name, Ent);
      end if;

      --  MECHANISM_NAME ::= value | reference | descriptor | short_descriptor

      if Nkind (Mech_Name) = N_Identifier then
         if Chars (Mech_Name) = Name_Value then
            Set_Mechanism_With_Checks (Ent, By_Copy, Mech_Name);
            return;

         elsif Chars (Mech_Name) = Name_Reference then
            Set_Mechanism_With_Checks (Ent, By_Reference, Mech_Name);
            return;

         elsif Chars (Mech_Name) = Name_Descriptor then
            Check_VMS (Mech_Name);
            Set_Mechanism_With_Checks (Ent, By_Descriptor, Mech_Name);
            return;

         elsif Chars (Mech_Name) = Name_Short_Descriptor then
            Check_VMS (Mech_Name);
            Set_Mechanism_With_Checks (Ent, By_Short_Descriptor, Mech_Name);
            return;

         elsif Chars (Mech_Name) = Name_Copy then
            Error_Msg_N ("bad mechanism name, Value assumed", Mech_Name);
            Set_Mechanism (Ent, By_Copy);

         else
            Bad_Mechanism;
            return;
         end if;

      --  MECHANISM_NAME ::= descriptor (CLASS_NAME) |
      --                     short_descriptor (CLASS_NAME)
      --  CLASS_NAME     ::= ubs | ubsb | uba | s | sb | a | nca

      --  Note: this form is parsed as an indexed component

      elsif Nkind (Mech_Name) = N_Indexed_Component then
         Class := First (Expressions (Mech_Name));

         if Nkind (Prefix (Mech_Name)) /= N_Identifier
           or else not (Chars (Prefix (Mech_Name)) = Name_Descriptor or else
                        Chars (Prefix (Mech_Name)) = Name_Short_Descriptor)
           or else Present (Next (Class))
         then
            Bad_Mechanism;
            return;
         end if;

      --  MECHANISM_NAME ::= descriptor (Class => CLASS_NAME) |
      --                     short_descriptor (Class => CLASS_NAME)
      --  CLASS_NAME     ::= ubs | ubsb | uba | s | sb | a | nca

      --  Note: this form is parsed as a function call

      elsif Nkind (Mech_Name) = N_Function_Call then

         Param := First (Parameter_Associations (Mech_Name));

         if Nkind (Name (Mech_Name)) /= N_Identifier
           or else not (Chars (Name (Mech_Name)) = Name_Descriptor or else
                        Chars (Name (Mech_Name)) = Name_Short_Descriptor)
           or else Present (Next (Param))
           or else No (Selector_Name (Param))
           or else Chars (Selector_Name (Param)) /= Name_Class
         then
            Bad_Mechanism;
            return;
         else
            Class := Explicit_Actual_Parameter (Param);
         end if;

      else
         Bad_Mechanism;
         return;
      end if;

      --  Fall through here with Class set to descriptor class name

      Check_VMS (Mech_Name);

      if Nkind (Class) /= N_Identifier then
         Bad_Class;
         return;

      elsif Chars (Name (Mech_Name)) = Name_Descriptor
        and then Chars (Class) = Name_UBS
      then
         Set_Mechanism_With_Checks (Ent, By_Descriptor_UBS,  Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Descriptor
        and then Chars (Class) = Name_UBSB
      then
         Set_Mechanism_With_Checks (Ent, By_Descriptor_UBSB, Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Descriptor
        and then Chars (Class) = Name_UBA
      then
         Set_Mechanism_With_Checks (Ent, By_Descriptor_UBA,  Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Descriptor
        and then Chars (Class) = Name_S
      then
         Set_Mechanism_With_Checks (Ent, By_Descriptor_S,    Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Descriptor
        and then Chars (Class) = Name_SB
      then
         Set_Mechanism_With_Checks (Ent, By_Descriptor_SB,   Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Descriptor
        and then Chars (Class) = Name_A
      then
         Set_Mechanism_With_Checks (Ent, By_Descriptor_A,    Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Descriptor
        and then Chars (Class) = Name_NCA
      then
         Set_Mechanism_With_Checks (Ent, By_Descriptor_NCA,  Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Short_Descriptor
        and then Chars (Class) = Name_UBS
      then
         Set_Mechanism_With_Checks (Ent, By_Short_Descriptor_UBS,  Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Short_Descriptor
        and then Chars (Class) = Name_UBSB
      then
         Set_Mechanism_With_Checks (Ent, By_Short_Descriptor_UBSB, Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Short_Descriptor
        and then Chars (Class) = Name_UBA
      then
         Set_Mechanism_With_Checks (Ent, By_Short_Descriptor_UBA,  Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Short_Descriptor
        and then Chars (Class) = Name_S
      then
         Set_Mechanism_With_Checks (Ent, By_Short_Descriptor_S,    Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Short_Descriptor
        and then Chars (Class) = Name_SB
      then
         Set_Mechanism_With_Checks (Ent, By_Short_Descriptor_SB,   Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Short_Descriptor
        and then Chars (Class) = Name_A
      then
         Set_Mechanism_With_Checks (Ent, By_Short_Descriptor_A,    Mech_Name);

      elsif Chars (Name (Mech_Name)) = Name_Short_Descriptor
        and then Chars (Class) = Name_NCA
      then
         Set_Mechanism_With_Checks (Ent, By_Short_Descriptor_NCA,  Mech_Name);

      else
         Bad_Class;
         return;
      end if;
   end Set_Mechanism_Value;

   -------------------------------
   -- Set_Mechanism_With_Checks --
   -------------------------------

   procedure Set_Mechanism_With_Checks
     (Ent  : Entity_Id;
      Mech : Mechanism_Type;
      Enod : Node_Id)
   is
   begin
      --  Right now we only do some checks for functions returning arguments
      --  by descriptor. Probably mode checks need to be added here ???

      if Mech in Descriptor_Codes and then not Is_Formal (Ent) then
         if Is_Record_Type (Etype (Ent)) then
            Error_Msg_N ("??records cannot be returned by Descriptor", Enod);
            return;
         end if;
      end if;

      --  If we fall through, all checks have passed

      Set_Mechanism (Ent, Mech);
   end Set_Mechanism_With_Checks;

   --------------------
   -- Set_Mechanisms --
   --------------------

   procedure Set_Mechanisms (E : Entity_Id) is
      Formal : Entity_Id;
      Typ    : Entity_Id;

   begin
      --  Skip this processing if inside a generic template. Not only is
      --  it unnecessary (since neither extra formals nor mechanisms are
      --  relevant for the template itself), but at least at the moment,
      --  procedures get frozen early inside a template so attempting to
      --  look at the formal types does not work too well if they are
      --  private types that have not been frozen yet.

      if Inside_A_Generic then
         return;
      end if;

      --  Loop through formals

      Formal := First_Formal (E);
      while Present (Formal) loop

         if Mechanism (Formal) = Default_Mechanism then
            Typ := Underlying_Type (Etype (Formal));

            --  If there is no underlying type, then skip this processing and
            --  leave the convention set to Default_Mechanism. It seems odd
            --  that there should ever be such cases but there are (see
            --  comments for filed regression tests 1418-001 and 1912-009) ???

            if No (Typ) then
               goto Skip_Formal;
            end if;

            case Convention (E) is

               ---------
               -- Ada --
               ---------

               --  Note: all RM defined conventions are treated the same
               --  from the point of view of parameter passing mechanism

               when Convention_Ada       |
                    Convention_Intrinsic |
                    Convention_Entry     |
                    Convention_Protected |
                    Convention_Stubbed   =>

                  --  By reference types are passed by reference (RM 6.2(4))

                  if Is_By_Reference_Type (Typ) then
                     Set_Mechanism (Formal, By_Reference);

                  --  By copy types are passed by copy (RM 6.2(3))

                  elsif Is_By_Copy_Type (Typ) then
                     Set_Mechanism (Formal, By_Copy);

                  --  All other types we leave the Default_Mechanism set, so
                  --  that the backend can choose the appropriate method.

                  else
                     null;
                  end if;

               --  Special Ada conventions specifying passing mechanism

               when Convention_Ada_Pass_By_Copy =>
                  Set_Mechanism (Formal, By_Copy);

               when Convention_Ada_Pass_By_Reference =>
                  Set_Mechanism (Formal, By_Reference);

               -------
               -- C --
               -------

               --  Note: Assembler, C++, Java, Stdcall also use C conventions

               when Convention_Assembler |
                    Convention_C         |
                    Convention_CIL       |
                    Convention_CPP       |
                    Convention_Java      |
                    Convention_Stdcall   =>

                  --  The following values are passed by copy

                  --    IN Scalar parameters (RM B.3(66))
                  --    IN parameters of access types (RM B.3(67))
                  --    Access parameters (RM B.3(68))
                  --    Access to subprogram types (RM B.3(71))

                  --  Note: in the case of access parameters, it is the pointer
                  --  that is passed by value. In GNAT access parameters are
                  --  treated as IN parameters of an anonymous access type, so
                  --  this falls out free.

                  --  The bottom line is that all IN elementary types are
                  --  passed by copy in GNAT.

                  if Is_Elementary_Type (Typ) then
                     if Ekind (Formal) = E_In_Parameter then
                        Set_Mechanism (Formal, By_Copy);

                     --  OUT and IN OUT parameters of elementary types are
                     --  passed by reference (RM B.3(68)). Note that we are
                     --  not following the advice to pass the address of a
                     --  copy to preserve by copy semantics.

                     else
                        Set_Mechanism (Formal, By_Reference);
                     end if;

                  --  Records are normally passed by reference (RM B.3(69)).
                  --  However, this can be overridden by the use of the
                  --  C_Pass_By_Copy pragma or C_Pass_By_Copy convention.

                  elsif Is_Record_Type (Typ) then

                     --  If the record is not convention C, then we always
                     --  pass by reference, C_Pass_By_Copy does not apply.

                     if Convention (Typ) /= Convention_C then
                        Set_Mechanism (Formal, By_Reference);

                     --  OUT and IN OUT parameters of record types are passed
                     --  by reference regardless of pragmas (RM B.3 (69/2)).

                     elsif Ekind_In (Formal, E_Out_Parameter,
                                             E_In_Out_Parameter)
                     then
                        Set_Mechanism (Formal, By_Reference);

                     --  IN parameters of record types are passed by copy only
                     --  when the related type has convention C_Pass_By_Copy
                     --  (RM B.3 (68.1/2)).

                     elsif Ekind (Formal) = E_In_Parameter
                       and then C_Pass_By_Copy (Typ)
                     then
                        Set_Mechanism (Formal, By_Copy);

                     --  Otherwise, for a C convention record, we set the
                     --  convention in accordance with a possible use of
                     --  the C_Pass_By_Copy pragma. Note that the value of
                     --  Default_C_Record_Mechanism in the absence of such
                     --  a pragma is By_Reference.

                     else
                        Set_Mechanism (Formal, Default_C_Record_Mechanism);
                     end if;

                  --  Array types are passed by reference (B.3 (71))

                  elsif Is_Array_Type (Typ) then
                     Set_Mechanism (Formal, By_Reference);

                  --  For all other types, use Default_Mechanism mechanism

                  else
                     null;
                  end if;

               -----------
               -- COBOL --
               -----------

               when Convention_COBOL =>

                  --  Access parameters (which in GNAT look like IN parameters
                  --  of an access type) are passed by copy (RM B.4(96)) as
                  --  are all other IN parameters of scalar type (RM B.4(97)).

                  --  For now we pass these parameters by reference as well.
                  --  The RM specifies the intent BY_CONTENT, but gigi does
                  --  not currently transform By_Copy properly. If we pass by
                  --  reference, it will be imperative to introduce copies ???

                  if Is_Elementary_Type (Typ)
                    and then Ekind (Formal) = E_In_Parameter
                  then
                     Set_Mechanism (Formal, By_Reference);

                  --  All other parameters (i.e. all non-scalar types, and
                  --  all OUT or IN OUT parameters) are passed by reference.
                  --  Note that at the moment we are not bothering to make
                  --  copies of scalar types as recommended in the RM.

                  else
                     Set_Mechanism (Formal, By_Reference);
                  end if;

               -------------
               -- Fortran --
               -------------

               when Convention_Fortran =>

                  --  In OpenVMS, pass a character of array of character
                  --  value using Descriptor(S).

                  if OpenVMS_On_Target
                    and then (Root_Type (Typ) = Standard_Character
                               or else
                                 (Is_Array_Type (Typ)
                                   and then
                                     Root_Type (Component_Type (Typ)) =
                                                     Standard_Character))
                  then
                     Set_Mechanism (Formal, By_Descriptor_S);

                  --  Access types are passed by default (presumably this
                  --  will mean they are passed by copy)

                  elsif Is_Access_Type (Typ) then
                     null;

                  --  For now, we pass all other parameters by reference.
                  --  It is not clear that this is right in the long run,
                  --  but it seems to correspond to what gnu f77 wants.

                  else
                     Set_Mechanism (Formal, By_Reference);
                  end if;

            end case;
         end if;

         <<Skip_Formal>> -- remove this when problem above is fixed ???

         Next_Formal (Formal);
      end loop;

      --  Note: there is nothing we need to do for the return type here.
      --  We deal with returning by reference in the Ada sense, by use of
      --  the flag By_Ref, rather than by messing with mechanisms.

      --  A mechanism of Reference for the return means that an extra
      --  parameter must be provided for the return value (that is the
      --  DEC meaning of the pragma), and is unrelated to the Ada notion
      --  of return by reference.

      --  Note: there was originally code here to set the mechanism to
      --  By_Reference for types that are "by reference" in the Ada sense,
      --  but, in accordance with the discussion above, this is wrong, and
      --  the code was removed.

   end Set_Mechanisms;

end Sem_Mech;
