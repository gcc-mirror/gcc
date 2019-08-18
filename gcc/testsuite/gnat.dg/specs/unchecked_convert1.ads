-- { dg-do compile }
-- { dg-options "-gnatws" }

with Ada.Unchecked_Conversion;
with System;

package Unchecked_Convert1 is

  type Rec (D : Boolean := False) is record
    case D is
      when True => I : Integer;
      when False => null;
    end case;
  end record;

  function To_Rec is new Ada.Unchecked_Conversion (System.Address, Rec);

  function F (A : System.Address) return Rec is (To_Rec (A));

end Unchecked_Convert1;
