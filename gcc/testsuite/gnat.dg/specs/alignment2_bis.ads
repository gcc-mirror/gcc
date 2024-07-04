-- { dg-do compile }

with Interfaces; use Interfaces;

package Alignment2_Bis is

  pragma Warnings (Off, "*size*");

  -- OK, big size
  type R3 is record
    A, B, C, D : Integer_8;
  end record;
  for R3'Size use 32 * 8;
  for R3'Alignment use 32;

  -- OK, big size
  type R4 is record
    A, B, C, D, E, F, G, H : Integer_32;
  end record;
  for R4'Alignment use 32;

  -- warning
  type I1 is new Integer_32;
  for I1'Size use 32;
  for I1'Alignment use 32; -- { dg-error "error: specified alignment too large for discrete or fixed point type" }

  -- warning
  type I2 is new Integer_32;
  for I2'Alignment use 32; -- { dg-error "error: specified alignment too large for discrete or fixed point type" }

  -- OK, big size
  type I3 is new Integer_32;
  for I3'Size use 32 * 8;
  for I3'Alignment use 32; -- { dg-error "error: specified alignment too large for discrete or fixed point type" }

end Alignment2_Bis;
