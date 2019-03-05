/* This is a copy of gcc/testsuite/gcc.c-torture/execute/pr56982.c to test the
   setjmp case of the bti pass.  */
/* { dg-do run } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target aarch64_bti_hw } */
/* { dg-options "--save-temps -mbranch-protection=standard" } */

#include <setjmp.h>

extern void abort (void);
extern void exit (int);

static jmp_buf env;

void baz (void)
{
  __asm__ volatile ("" : : : "memory");
}

static inline int g(int x)
{
    if (x)
    {
        baz();
        return 0;
    }
    else
    {
        baz();
        return 1;
    }
}

int f(int *e)
{
    if (*e)
      return 1;

    int x = setjmp(env);
    int n = g(x);
    if (n == 0)
      exit(0);
    if (x)
      abort();
    longjmp(env, 42);
}
/* { dg-final { scan-assembler "hint\t36" } } */

int main(int argc, char** argv)
{
    int v = 0;
    return f(&v);
}
