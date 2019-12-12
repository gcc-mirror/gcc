--  { dg-do compile }

package body Addr13 is
   procedure Overlay is
      Over : Integer with Address => Gen_Obj'Address;
   begin
      Over := 123;
   end Overlay;
end Addr13;
