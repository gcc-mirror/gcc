// { dg-additional-options "-fmodules-ts" }
module bob;

int Frob (int n, int a)
{
  return frob (n, a);
}

// { dg-final { scan-assembler "_ZW3bobE4frobii:" } }
// { dg-final { scan-assembler ".weak(_definition)?\[\t ]*_?_ZW3bobE4frobii" } }
// { dg-final { scan-assembler "_Z4Frobii:" } }
