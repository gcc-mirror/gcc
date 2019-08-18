--  { dg-do compile }

with Ada.Unchecked_Deallocation;

package body Task3 is
   procedure Destroy (Obj : in out Child_Wrapper) is
      procedure Free is new Ada.Unchecked_Deallocation (Child, Child_Ptr);
   begin
      Free (Obj.Ptr);
   end Destroy;
end Task3;
