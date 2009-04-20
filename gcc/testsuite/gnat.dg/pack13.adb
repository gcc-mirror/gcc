-- [ dg-do compile }

package body Pack13 is

  procedure Set (Myself : Object_Ptr; The_Data : Thirty_Two_Bits.Object) is
  begin
    Myself.Something.Data_1 := The_Data;
  end;

end Pack13;
