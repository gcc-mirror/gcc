/* PR rtl-optimization/119291 */

int a;
long c;

__attribute__((noipa)) void
foo (int x)
{
  if (x != 0)
    __builtin_abort ();
  a = 42;
}

int
main ()
{
  int e = 1;
lab:
  if (a < 2)
    {
      int b = e;
      _Bool d = a != 0;
      _Bool f = b != 0;
      unsigned long g = -(d & f);
      unsigned long h = c & g;
      unsigned long i = ~c;
      e = -(i & h);
      c = e != 0;
      a = ~e + b;
      foo (e);
      goto lab;
    }
}
