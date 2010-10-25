with Ada.Finalization;

package Loop_Optimization8_Pkg1 is

  type Array_T is array (Positive range <>) of Natural;

  type Array_Access_T is access Array_T;

  type T is new Ada.Finalization.Controlled with record
    Last : Natural := 0;
    Elements : Array_Access_T;
  end record;

  Empty : T := (Ada.Finalization.Controlled with Last => 0, Elements => null);

  generic
    with procedure Action (Info : Natural);
  procedure Iter;

end Loop_Optimization8_Pkg1;
