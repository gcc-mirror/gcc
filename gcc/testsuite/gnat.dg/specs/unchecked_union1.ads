-- PR ada/28591
-- Reported by Martin Michlmayr <tbm@cyrius.com>

-- { dg-do compile }
-- { dg-options "-g" }

with Interfaces; use Interfaces;

package Unchecked_Union1 is
   type Mode_Type is (Mode_B2);

   type Value_Union (Mode : Mode_Type := Mode_B2) is record
      case Mode is
         when Mode_B2 =>
            B2 : Integer_32;
      end case;
   end record;
   pragma Unchecked_Union (Value_Union);

end Unchecked_Union1;
