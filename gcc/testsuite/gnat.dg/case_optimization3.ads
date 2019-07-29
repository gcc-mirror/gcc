package Case_Optimization3 is

   type T_UINT32 is range 0 .. (2 ** 32) - 1;
   for T_UINT32'Size use 32;

   subtype T_RANGE is T_UINT32 range 0 .. 7;

   procedure Proc (Val : T_RANGE);

end Case_Optimization3;
