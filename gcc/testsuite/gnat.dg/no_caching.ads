package No_Caching with SPARK_Mode is
   type Mult_Bit_Boolean is (NV_FALSE, NV_TRUE);
   for Mult_Bit_Boolean use (NV_FALSE => 16#55_AA#,
                             NV_TRUE => 16#AA_55#);
   procedure Handle (V : Mult_Bit_Boolean);
   procedure Do_Something;
   procedure Do_Something_Else;
end No_Caching;
