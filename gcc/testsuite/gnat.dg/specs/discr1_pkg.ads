package Discr1_Pkg is

  Maximum_Length : Natural := 80 ;

  subtype String_Length is Natural range 0 .. Maximum_Length;

  type Variable_String (Length : String_Length := 0) is
    record
      S : String (1 .. Length);
    end record;

  type Variable_String_Array is array (Natural range <>) of Variable_String;

end Discr1_Pkg;
