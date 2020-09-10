with Discr59_Pkg2;

package Discr59_Pkg1 is

   subtype Index_Type is Natural range 1 .. 300;

   type Code_Type is (Global_Query, Status_Query, Alert);

   type Id_Type is (None, At_Command, At_Response);

   package My_G is new Discr59_Pkg2 (21);

   type Arr is array (Index_Type range <>) of My_G.Token_Type;

   type Unit_List_Type (Last : Natural) is record
      A : Arr (1 .. Last);
   end record;

   type At_Response_Type (Kind : Code_Type; Units : Natural) is record
      case Kind is
         when Global_Query => Global_Query : Unit_List_Type (Units);
         when Status_Query => null;
         when Alert        => Alert : Unit_List_Type (Units);
      end case;
   end record;

   type Rec (Kind : Id_Type; Code : Code_Type; Units : Natural) is record
      case Kind is
         when None        => null;
         when At_Command  => null;
         when At_Response => At_Response : At_Response_Type (Code, Units);
      end case;
   end record;

end Discr59_Pkg1;
