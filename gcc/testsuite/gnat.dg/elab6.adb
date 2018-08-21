--  { dg-do compile }
--  { dg-options "-gnatE" }

package body Elab6 is
   procedure Force_Body is null;

   package Inst is new Elab6_Pkg;
end Elab6;
