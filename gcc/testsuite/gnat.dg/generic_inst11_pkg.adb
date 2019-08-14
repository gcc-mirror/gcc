with System;

package body Generic_Inst11_Pkg is

   Data : Integer;

   generic
      Reg_Address : System.Address;
   procedure Inner_G with Inline;

   procedure Inner_G is
      Reg : Integer with Address => Reg_Address;
   begin
      null;
   end;

   procedure My_Inner_G is new Inner_G (Data'Address);

   procedure Proc renames My_Inner_G;

end Generic_Inst11_Pkg;
