--  { dg-do compile }

with text_io; use text_io;
procedure modify_a_constant is
    type Outer;
    type Inner (Outer_Ref : access Outer) is limited null record;
        
    type Outer is limited record
        Inner_Field   : Inner (Outer_Ref => Outer'Access);
        Integer_Field : Integer;
      end record;
     
    X : constant Outer := (Inner_Field => <>, Integer_Field => 123);
     
begin
   Put_Line (Integer'image (X.Integer_Field));
   X.Inner_Field.Outer_Ref.Integer_Field := 0;
   Put_Line (Integer'image (X.Integer_Field));
end Modify_A_Constant;
