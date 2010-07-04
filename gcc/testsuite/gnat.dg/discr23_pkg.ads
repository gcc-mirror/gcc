package Discr23_Pkg is

  subtype Size_Range is Positive range 1 .. 256;

  type Text (Size : Size_Range) is
    record
      Characters : String( 1.. Size);
    end record;

  function Get return Text;

end Discr23_Pkg;
