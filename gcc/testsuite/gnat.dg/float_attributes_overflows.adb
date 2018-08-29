--  { dg-do run }

procedure Float_Attributes_Overflows is

   generic
      type Float_Type is digits <>;
   procedure Test_Float_Type;

   procedure Test_Float_Type is
       Biggest_Positive_float : Float_Type := Float_Type'Last;
       Biggest_Negative_Float : Float_Type := Float_Type'First;
       Float_Var : Float_Type;

    begin
       begin
             Float_Var := Float_Type'succ (Biggest_Positive_Float);
             raise Program_Error;
       exception
          when Constraint_Error => null;
       end;

       begin
             Float_Var := Float_Type'pred (Biggest_Negative_Float);
             raise Program_Error;
       exception
          when Constraint_Error => null;
       end;
   end Test_Float_Type;

   procedure Test_Float is new Test_Float_Type (Float);
   procedure Test_Long_Float is new Test_Float_Type (Long_Float);
begin
   Test_Float;
   Test_Long_Float;
end Float_Attributes_Overflows;
