/* Test that structs returned in registers do not lead to
   instrumentation with -fstack-protector-strong.  */

/* { dg-do compile { target { ! { ia32 } } } } */
/* { dg-options "-O2 -fstack-protector-strong" } */

struct S {
  int a;
  int b;
};

struct S f (void);

int g (void)
{
  return f ().a;
}

/* { dg-final { scan-assembler-times "stack_chk_fail" 0 } } */
