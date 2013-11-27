/* PR middle-end/59138 */
/* Testcase by John Regehr <regehr@cs.utah.edu> */

extern void abort (void);

#pragma pack(1)

struct S0 {
  int f0;
  int f1;
  int f2;
  short f3;
};

short a = 1;

struct S0 b = { 1 }, c, d, e;

struct S0 fn1() { return c; }

void fn2 (void)
{
  b = fn1 ();
  a = 0;
  d = e;
}

int main (void)
{
  fn2 ();
  if (a != 0)
    abort ();
  return 0;
}
