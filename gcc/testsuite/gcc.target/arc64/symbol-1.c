/* { dg-options "-O2 -fPIC" } */

struct {
  int a;
  int b;
} _rtld_local;

extern void bar (int *);
void foo (void)
{
  bar(&_rtld_local.b);
}

/* { dg-final { scan-assembler "_rtld_local@gotpc" } } */
