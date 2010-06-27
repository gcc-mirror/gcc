package Noreturn3 is

  Exc1 : Exception;
  Exc2 : Exception;
  Exc3 : Exception;

  type Enum is (One, Two, Three);

  procedure Raise_Error (E : Enum; ErrorMessage : String);
  pragma No_Return (Raise_Error);

end Noreturn3;
