package Address_Null_Init is
   
   type Acc is access Integer;
   A : Acc := new Integer'(123);
   B : Acc;  -- Variable must be set to null (and A overwritten by null)
   for B'Address use A'Address;

end Address_Null_Init;
