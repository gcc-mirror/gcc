with Ada.Finalization; use Ada.Finalization;

package Lto6_Pkg is
  type F_String is new Controlled with record
    Data : access String;
  end record;
  Null_String : constant F_String := (Controlled with Data => null);
end Lto6_Pkg;
