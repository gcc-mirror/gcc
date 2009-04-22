with Ada.Finalization; use Ada.Finalization;

package Enum2_Pkg is
  type F_String is new Controlled with record
    Data : access String;
  end record;
  Null_String : constant F_String := (Controlled with Data => null);
end Enum2_Pkg;
