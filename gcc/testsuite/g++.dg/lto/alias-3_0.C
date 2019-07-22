/* { dg-lto-do run } */
/* { dg-lto-options { { -O3 -flto -fno-early-inlining } } } */

struct a
{
  int foo,bar;
};
struct b
{
  struct a a[10];
};

__attribute__ ((used)) struct b b, *bptr=&b, *bptr2=&b;
__attribute__ ((used)) int i,j;

extern "C" void inline_me_late (void);
int n=1;

int
main (void)
{
  int jj=j;
  bptr2->a[jj].bar = 0;
  for (int i=0; i<n; i++)
    inline_me_late ();
  if (!__builtin_constant_p (bptr2->a[jj].bar == 0))
    __builtin_abort ();
  return 0;
}
