generic
package Generic_Inst11_Pkg.G1.Parser.G2.Parser is
   type Instance is new Generic_Inst11_Pkg.G1.Parser.Instance
     with null record;
   overriding
   procedure Parse (Parser : in out Instance) is null;
end Generic_Inst11_Pkg.G1.Parser.G2.Parser;
