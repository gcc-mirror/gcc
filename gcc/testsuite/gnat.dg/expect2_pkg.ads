package Expect2_Pkg is

  I : Integer;
  pragma Volatile (I);

  function Expect (Condition : Boolean; Outcome : Boolean) return Boolean;
  pragma Import (Intrinsic, Expect, "__builtin_expect");

  function Likely (Condition : Boolean) return Boolean;
  pragma Import (Intrinsic, Likely, "__builtin_likely");

  function Unlikely (Condition : Boolean) return Boolean;
  pragma Import (Intrinsic, Unlikely, "__builtin_unlikely");

end Expect2_Pkg;
