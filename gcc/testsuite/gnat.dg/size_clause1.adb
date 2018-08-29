procedure Size_Clause1 is

  type Modular is mod 2**64;
  for Modular'Size use 64;

  subtype Enlarged_Modular is Modular;
  for Enlarged_Modular'Object_Size use 128; --  { dg-warning "warning: 64 bits of \"Enlarged_Modular\" unused" }

begin
    null;
end Size_Clause1;
