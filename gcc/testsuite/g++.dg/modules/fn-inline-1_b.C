module bob;

int Frob (int n, int a)
{
  return frob (n, a);
}

// { dg-final { scan-assembler "_ZW3bobE4frobii:" } }
// { dg-final { scan-assembler ".text._ZW3bobE4frobii,.*,_ZW3bobE4frobii,comdat" } }
// { dg-final { scan-assembler "_Z4Frobii:" } }
