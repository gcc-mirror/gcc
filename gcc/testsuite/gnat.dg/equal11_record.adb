with Ada.Text_IO;

package body Equal11_Record is

  procedure Put (R : in My_Record_Type) is
  begin
    Put_Result := R.F;
  end Put;

end Equal11_Record;
