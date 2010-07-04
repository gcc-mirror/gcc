// { dg-do compile }
// { dg-options "-Wunused" }

template <int N>
int
f1 (void)
{
  int c = ({
    int a;
    a = 1;
    a; });
  return c;
}

template <int N>
void
f2 (void)
{
  int f;
  f = 0;
  __asm__ __volatile__ ("" : "+r" (f));
}

void
test ()
{
  (void) f1<0> ();
  f2<0> ();
}
