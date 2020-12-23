// { dg-additional-options "-fmodules-ts" }
import bob;

int main ()
{
  if (frob (2) != -2)
    return 1;
  if (Frob (0, 2) != 2)
    return 1;
  if (Frob (2, 2) != 8)
    return 1;
  return 0;
}

// { dg-final { scan-assembler "_Z4frobi:" } }
// { dg-final { scan-assembler ".weak(_definition)?\[\t ]*_?_Z4frobi" } }
