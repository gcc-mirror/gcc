--  { dg-do compile }

with Elab3_Pkg;

package body Elab3 is
   package Inst is new Elab3_Pkg (False, ABE);

   procedure ABE is begin null; end ABE;
end Elab3;
