package Atomic6_Pkg is

  type Int is new Integer;
  pragma Atomic (Int);

  Counter1 : Int;
  Counter2 : Int;

  Timer1 : Integer;
  pragma Atomic (Timer1);

  Timer2 : Integer;
  pragma Atomic (Timer2);

  type Arr1 is array (1..8) of Int;
  Counter : Arr1;

  type Arr2 is array (1..8) of Integer;
  pragma Atomic_Components (Arr2);
  Timer : Arr2;

  type R is record
    Counter1 : Int;
    Timer1 : Integer;
    pragma Atomic (Timer1);
    Counter2 : Int;
    Timer2 : Integer;
    pragma Atomic (Timer2);
    Dummy : Integer;
  end record;

  type Int_Ptr is access all Int;

end Atomic6_Pkg;
