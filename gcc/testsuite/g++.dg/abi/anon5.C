// anon-union decls may need a discriminator

void f ()
{
  { static int bob; }
  { static union {int bob;};}
  { static int bob; }
  { static union {int bob;}; }
}

// { dg-final { scan-assembler {_ZZ1fvE3bob[^_]} } }
// { dg-final { scan-assembler {_ZZ1fvE3bob_0} } }
// { dg-final { scan-assembler {_ZZ1fvE3bob_1} } }
// { dg-final { scan-assembler {_ZZ1fvE3bob_2} } }
