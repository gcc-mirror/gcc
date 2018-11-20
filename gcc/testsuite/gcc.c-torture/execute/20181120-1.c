/* PR rtl-optimization/85925 */
/* Testcase by <sudi@gcc.gnu.org> */

int a, c, d;
volatile int b;
int *e = &d;

union U1 {
  unsigned f0;
  unsigned f1 : 15;
};

int main (void)
{
  for (c = 0; c <= 1; c++) {
    union U1 f = {0x10101};
    if (c == 1)
      b;
    *e = f.f1;
  }

  if (d != 0x101)
    __builtin_abort ();

  return 0;
}
