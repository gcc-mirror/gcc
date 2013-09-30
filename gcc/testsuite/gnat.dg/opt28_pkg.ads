package Opt28_Pkg is

  type Posix_String is array (Positive range <>) of aliased Character;

  function To_Posix (Str : String) return Posix_String;
  function To_String (Str : Posix_String) return String;
  function Is_File (Str : Posix_String) return Boolean;
  function Value_Of (Name : Posix_String) return Posix_String;
  function Position (In_Line : String) return Natural;

end Opt28_Pkg;
