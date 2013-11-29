package Opt29 is

  type Word is mod 2**16;

  type PID is record
    W1, W2: Word;
  end record;

  type Root1 is tagged record
    Id: PID;
  end record;
  type Root1_Ptr is access all Root1'Class;

  type Root2 is tagged null record;
  type Root2_Ptr is access all Root2'class;

  type Derived2 is new Root2 with record
    Id: PID;
  end record;

  type Rec is record
    F1: Root1_Ptr;
    F2: Root2_Ptr;
  end record;

  procedure Proc (T : Rec);

end Opt29;
