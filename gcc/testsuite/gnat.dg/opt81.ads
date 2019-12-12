package Opt81 is

  type String_Access is access String;

  type Rec is record
    A : String_Access;
  end record;

  for Rec use record
    A at 0 range 0 .. (Standard'Word_Size - 1);
  end record;

  procedure Copy(From, To : Rec);

end Opt81;
