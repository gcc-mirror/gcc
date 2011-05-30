-- { dg-do compile }
-- { dg-options "-O3 -gnata" }

package body Boolean_Subtype2 is

   function Component_Type (Id : Entity_Id) return Entity_Id is
   begin
      pragma Assert (Is_String_Type (Id));
      return Node20 (Id);
   end;

   function First_Index (Id : Entity_Id) return Node_Id is
   begin
      pragma Assert (Is_String_Type (Id));
      return Node20 (Id);
   end ;

   function Is_Character_Type (Id : Entity_Id) return B is
   begin
      return Flag63 (Id);
   end;

   function Number_Dimensions (Id : Entity_Id) return Positive is
      N : Integer := 0;
      T : Node_Id := First_Index (Id);
   begin
      if Present (T) then
         N := N + 1;
      end if;
      return N;
   end;

   function Is_String_Type (Id : Entity_Id) return B is
   begin
      return (Id /= 0
              and then Number_Dimensions (Id) = 1
              and then Is_Character_Type (Component_Type (Id)));
   end;

end Boolean_Subtype2;
