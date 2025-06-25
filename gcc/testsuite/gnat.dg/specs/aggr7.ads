-- { dg-do compile }

package Aggr7 is

  type Arr is array (Integer range <>) of Boolean;

  Data : constant Arr := (False, True);

  function Get_Data return Arr is (Data);

end Aggr7;
