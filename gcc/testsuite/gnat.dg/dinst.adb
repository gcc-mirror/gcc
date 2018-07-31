-- { dg-do compile { target *-*-gnu* } }
-- { dg-options "-gnateS -gdwarf -g -O -gno-column-info" }
-- { dg-final { scan-assembler "loc \[0-9] 5 \[0-9]( is_stmt \[0-9])? discriminator 1\n" } } */
-- { dg-final { scan-assembler-not "loc \[0-9] 5 \[0-9]( is_stmt \[0-9])? discriminator 2\n" } } */
-- { dg-final { scan-assembler "loc \[0-9] 5 \[0-9]( is_stmt \[0-9])? discriminator 3\n" } } */
-- { dg-final { scan-assembler "loc \[0-9] 5 \[0-9]( is_stmt \[0-9])? discriminator 4\n" } } */


with DInst_Pkg;
procedure DInst is
   package I1 is new DInst_Pkg; -- instance 1 
   package I2 is new DInst_Pkg; -- instance 2
   package I3 is new DInst_Pkg; -- instance 3
   package I4 is new DInst_Pkg; -- instance 4
begin
   I1.Foo;
   -- I2.Foo;
   I3.Foo;
   I4.Foo;
end;
