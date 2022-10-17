with Ada.Unchecked_Conversion;
with System;

package Opt98 is

  type Rec is record
    I : Integer;
  end record;

  function To_Address is new Ada.Unchecked_Conversion (Rec, System.Address);

  function To_Rec is new Ada.Unchecked_Conversion (System.Address, Rec);

  A : System.Address with Atomic;

  function Func return Rec;

end Opt98;

