
export module bob;
// { dg-module-bmi "bob" }

export inline int frob (int a)
{
  return -a;
}

inline int frob (int s, int a)
{
  while (s--)
    a <<= 1;
  return a;
}

export int Frob (int s, int a);

// { dg-final { scan-assembler-not "_Z4frobi:" } }
// { dg-final { scan-assembler-not "_ZW3bobE4frobii:" } }
