-- { dg-do compile }

package Atomic1 is

  type Arr is array (Integer range <>) of Boolean;
  type UA is access all Arr;

  U : UA;
  pragma Atomic (U);  -- { dg-error "atomic access" "" { xfail mips*-*-* { { i?86-*-* x86_64-*-* } && x32 } } }

  type R is record
    U : UA;
    pragma Atomic (U);  -- { dg-error "atomic access" "" { xfail mips*-*-* { { i?86-*-* x86_64-*-* } && x32 } } }
  end record;

end Atomic1;
