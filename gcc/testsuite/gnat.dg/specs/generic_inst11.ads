-- { dg-do compile }

with Generic_Inst11_Pkg.G1;
with Generic_Inst11_Pkg.G1.Parser;
with Generic_Inst11_Pkg.G1.Parser.G2;
with Generic_Inst11_Pkg.G1.Parser.G2.Parser;

package Generic_Inst11 is
  package My_G1        is new Generic_Inst11_Pkg.G1;
  package My_Parser    is new My_G1.Parser;
  package My_G2        is new My_Parser.G2;
  package My_G2_Parser is new My_G2.Parser;
end Generic_Inst11;
