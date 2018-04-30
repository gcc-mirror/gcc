with Ada.unchecked_conversion;

package Validity_Check2_Pkg is

  type Op_Code is (One, Two, Three, Four);

  subtype Valid_Msg is Integer range 0 .. 15;

  function Op_Code_To_Msg is
    new Ada.Unchecked_Conversion (Source => Op_code, Target => Valid_Msg);

  type Rec is record
    Code : Op_Code;
  end record;

end Validity_Check2_Pkg;
