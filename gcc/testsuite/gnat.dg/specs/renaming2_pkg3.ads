-- { dg-excess-errors "no code generated" }

generic

  type Base_Index_T is range <>;

  type Value_T is private;

package Renaming2_Pkg3 is

  type T is private;

  subtype Length_T is Base_Index_T range 0 .. Base_Index_T'Last;

  function Value (L : Length_T) return Value_T;

  function Next return Length_T;

private

  type Obj_T is null record;

  type T is access Obj_T;

end Renaming2_Pkg3;
