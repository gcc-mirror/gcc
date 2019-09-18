--  { dg-options "-gnatws" }

with Ada.Finalization;

package body Access8_Pkg is

   overriding procedure Initialize (O : in out Object) is
   begin
      null;
   end;

   overriding procedure Finalize (O : in out Object) is
   begin
      null;
   end;

   function Get return Object is
   begin
      return O : Object := Object'
        (Ada.Finalization.Limited_Controlled
          with D => new discriminant);
   end;

   function Get_Access return access Object is
   begin
      return new Object'
        (Ada.Finalization.Limited_Controlled
          with D => new Discriminant);
   end;
end;
