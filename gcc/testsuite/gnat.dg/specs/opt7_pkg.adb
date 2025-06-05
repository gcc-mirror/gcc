package body Opt7_Pkg is

  type Constant_String_Access is access constant String;

  type Enum_Name is array (Enum) of Constant_String_Access;

  Enum_Name_Table : constant Enum_Name :=
    (A => new String'("A"), B => new String'("B"));

  function Image (E : Enum) return String is
  begin
    return Enum_Name_Table (E).all;
  end Image;

end Opt7_Pkg;
