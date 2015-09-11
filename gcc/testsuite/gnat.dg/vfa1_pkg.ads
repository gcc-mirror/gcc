package VFA1_Pkg is

  type Int8_t is mod 2**8;

  type Int is new Integer;
  pragma Volatile_Full_Access (Int);

  Counter1 : Int;

  Counter2 : Integer;
  pragma Volatile_Full_Access (Counter2);

  type Arr is array (1 .. 4) of Int8_t;
  for Arr'Alignment use 4;
  pragma Volatile_Full_Access (Arr);

  Timer1 : Arr;

  Timer2 : array (1 .. 4) of Int8_t;
  for Timer2'Alignment use 4;
  pragma Volatile_Full_Access (Timer2);

  type Rec is record
    A : Short_Integer;
    B : Short_Integer;
  end record;

  type Rec_VFA is new Rec;
  pragma Volatile_Full_Access (Rec_VFA);

  Buffer1 : Rec_VFA;

  Buffer2 : Rec;
  pragma Volatile_Full_Access (Buffer2);

  type Code is record
    R : Int8_t;
    I : Int8_t;
  end record;
  pragma Volatile_Full_Access (Code);

  type CArr is array (1 .. 2) of Code;
  pragma Volatile_Full_Access (CArr);

  Mixer1 : Carr;

  Mixer2 :  array (1 .. 2) of Code;
  pragma Volatile_Full_Access (Mixer2);

end VFA1_Pkg;
