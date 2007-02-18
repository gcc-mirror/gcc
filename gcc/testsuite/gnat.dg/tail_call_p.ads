package Tail_Call_P is

  type T is new Natural;

  type Index is (First, Second);

  type A is array (Index) of T;

  My_Array : A := (0, 0);

  procedure Insert (Into : A; Element : T; Value : T);

end Tail_Call_P;
