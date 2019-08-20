--  { dg-do run }

procedure Null_Check with SPARK_Mode is
   type Int_Ptr is access Integer;
   subtype Not_Null_Int_Ptr is not null Int_Ptr;

   procedure Set_To_Null (X : out Int_Ptr) with Global => null is
   begin
      X := null;
   end Set_To_Null;

   X : Not_Null_Int_Ptr := new Integer'(12);
begin
   Set_To_Null (X);
   raise Program_Error;
exception
   when Constraint_Error =>
      null;
end Null_Check;
