/* PR rtl-optimization/9771 */
/* { dg-do run } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O2 -fomit-frame-pointer -ffixed-ebp" } */

extern void abort(void);
extern void exit(int);

register long *B asm ("ebp");

long x = 10;
long y = 20;

void bar(void)
{
  B = &y;
}

void foo()
{
  long *adr = B;
  long save = *adr;

  *adr = 123;

  bar();

  *adr = save;
}

int main()
{
  B = &x;

  foo();

  if (x != 10 || y != 20)
    abort();

  /* We can't return, as our caller may assume %ebp is preserved!  */
  /* We could save/restore it (like foo), but its easier to exit.  */
  exit(0);
}

