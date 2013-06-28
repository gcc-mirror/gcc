-- { dg-do run { target *-*-solaris2.* } }
-- { dg-options "-ftrapping-math" }

procedure FP_Exception is

  type my_fixed is digits 15;
  for my_fixed'size use 64;
  fixed1 : my_fixed := 1.0;  
  fixed2 : my_fixed := -0.0;
  mask_all : constant integer := 16#1F#;

  procedure fpsetmask(mask : in integer);
  pragma IMPORT (C, fpsetmask, "fpsetmask");

begin 

  -- Mask all floating point exceptions so they can be trapped
  fpsetmask (mask_all);

  fixed1 := fixed1 / fixed2;

exception
  when others => null;
end;
