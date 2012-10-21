-- { dg-do compile }

package Atomic1 is

  type Arr is array (Integer range <>) of Boolean;
  type UA is access all Arr;

  U : UA;
  pragma Atomic (U);  -- { dg-error "atomic access" "" { xfail mips*-*-* } }

  type R is record
    U : UA;
    pragma Atomic (U);  -- { dg-error "atomic access" "" { xfail mips*-*-* } }
  end record;

end Atomic1;
