with Renaming8_Pkg3; use Renaming8_Pkg3;

package Renaming8_Pkg2 is

  type Arr is array (Positive range 1 .. Last_Index) of Boolean;

  type Rec is record
     E : Arr;
  end record;

  function F return Rec;

end Renaming8_Pkg2;
