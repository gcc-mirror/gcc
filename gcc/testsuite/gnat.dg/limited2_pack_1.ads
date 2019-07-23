package Limited2_Pack_1 is
   type A is limited private;
   type A_Ptr is access all A;

private
   type B;
   type A is access all B;
end Limited2_Pack_1;
