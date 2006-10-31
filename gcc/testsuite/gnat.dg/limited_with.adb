-- { dg-do compile }

with Pack1;
package body limited_with is
   procedure Print_2 (Obj : access Pack1.Nested.Rec_Typ) is
   begin
      null;
   end;
end limited_with;
