-- { dg-do compile }
-- { dg-options "-g" }

with Ada.Unchecked_Conversion;
with System;

package body Debug3 is

  type Rec is record
     I : Integer;
  end record;
  for Rec'Alignment use 1;

  type Ptr is access Rec;

  function To_Ptr is new Ada.Unchecked_Conversion(System.Address, Ptr);

  procedure Proc is

    function Get (S1 : String) return Ptr is
    begin
       return To_Ptr (S1'Address);
    end;

    M : Ptr;

    begin
      M := Get ("");
    end;

end Debug3;
