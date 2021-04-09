// { dg-additional-options "-fmodules-ts" }
import "macro-1_a.H";

#define baz = + 1
int foo;
struct X 
{
  const char *s;
  int v;
}
;
X x kevin (5);

int main ()
{
  if (foo != 1)
    return 1;
  if (x.v != 5)
    return 2;
  const char *banana = "banana";
  for (unsigned ix = 0; banana[ix]; ix++)
    if (banana[ix] != x.s[ix])
      return 3;
  return 0;
}
