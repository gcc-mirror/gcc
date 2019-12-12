package Generic_Inst10_Pkg is

   type XString is tagged private;

   function To_String (S : XString) return String;

private

   type XString is tagged null record;

end Generic_Inst10_Pkg;
