-- { dg-do compile }

package Atomic2 is

  type Rec1 is record
    C : Character;
    I : Integer;
    pragma Atomic (I);
  end record;
  for Rec1 use record
    C at 0 range 0 .. 7;
    I at 1 range 0 .. 31; -- { dg-error "position of atomic field" }
  end record;

  type Rec2 is record
    C : Character;
    I : Integer;
    pragma Atomic (I);
  end record;
  pragma Pack (Rec2);

  type My_Int is new Integer;
  for My_Int'Alignment use 1;
  pragma Atomic (My_Int); -- { dg-error "atomic access" }

end Atomic2;
