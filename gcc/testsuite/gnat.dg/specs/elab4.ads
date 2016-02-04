-- { dg-do compile }

pragma Restrictions(No_Elaboration_Code);

with Elab4_Proc;

package Elab4 is

  procedure My_G is new Elab4_Proc;

end Elab4;

-- { dg-final { scan-assembler-not "elabs" } }
