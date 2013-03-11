package Loop_Optimization15 is

  type B16_T is mod 2 ** 16;
  for B16_T'Size use 16;
  for B16_T'Alignment use 1;

  procedure Proc (L : B16_T);

end Loop_Optimization15;
