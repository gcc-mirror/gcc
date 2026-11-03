-- { dg-do compile }
-- { dg-options "-O2" }

with Ada.Finalization;

package Vect1 is

  type Rec is new Ada.Finalization.Controlled with record
    Data : Integer;
  end record;

  type Arr is array (Boolean, Boolean, Boolean) of Rec;

end Vect1;
