// { dg-final { scan-assembler-not "_ZTHN1A1iE" } }
// { dg-final { scan-assembler-not "_ZTWN1A1iE" } }
// { dg-require-effective-target tls }

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
