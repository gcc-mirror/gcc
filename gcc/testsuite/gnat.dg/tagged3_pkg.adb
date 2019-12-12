with Ada.Text_IO; use Ada.Text_IO;
package body Tagged3_Pkg is
   procedure Prim1 (Self : access Parent) is
   begin
      raise Program_Error;
   end;

   procedure Prim1 (Self : access Child) is
   begin
     Child_Prim1_Called := True;
   end;
end;
