--  { dg-do compile }
--  { dg-options "-gnatJ" }
package body ABE_Pkg is
   package body Gen is
      procedure Force_Body is begin null; end Force_Body;
   begin
      ABE;
   end Gen;

   package Inst is new Gen;

   procedure ABE is begin null; end ABE;
end ABE_Pkg;
