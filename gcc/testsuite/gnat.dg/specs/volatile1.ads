-- { dg-do compile }

package Volatile1 is

  C : Character;
  for C'Size use 32;
  pragma Volatile (C);

  type R1 is record
    C: Character;
    pragma Volatile (C);
  end record;
  for R1 use record
    C at 0 range 0 .. 31;
  end record;

  type R2 is record
    C: Character;
    pragma Volatile (C);
  end record;
  for R2 use record
    C at 0 range 0 .. 10; -- { dg-error "size of volatile field" }
  end record;

end Volatile1;
