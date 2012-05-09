-- { dg-excess-errors "no code generated" }

generic

  type Length_T is range <>;

  with function Next return Length_T is <>;

  type Value_T is private;

  with function Value (L : Length_T) return Value_T is <>;

package Renaming2_Pkg4 is

  generic
    type T is private;
  package Inner is

    type Slave_T is tagged null record;

    function Next_Value return Value_T;

  end Inner;

end Renaming2_Pkg4;
