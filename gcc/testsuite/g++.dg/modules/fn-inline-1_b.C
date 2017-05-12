import bob;

int main ()
{
  return frob (2) != -2;
}

// { dg-final { scan-assembler "_Z4frobi:" } }
// { dg-final { scan-assembler ".text._Z4frobi,.*,_Z4frobi,comdat" } }
