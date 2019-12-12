package Global2 is
   type Int_Acc is access Integer;
   X : constant Int_Acc := new Integer'(34);
   procedure Change_X with Global => (In_Out => X);
   procedure Change2_X with Depends => (X => X);
end Global2;
