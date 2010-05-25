// { dg-skip-if "Additional Source File"  *-*-* "*" ""  }
// This is the additional source file for test static-1.mm

struct A
{
  static __thread int i;
};

int
test ()
{
  if (A::i != 8)
    return 1;

  A::i = 17;
  return 0;
}
