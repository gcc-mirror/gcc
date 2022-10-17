/* { dg-do compile { target { { { i?86-*-* x86_64-*-* } && lp64 } || { arm_thumb2 } } } } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue"  } */

int foo (int *p1, int *p2);

int
test (int *p1, int *p2)
{
  int *p;

  for (p = p2; p != 0; p++)
    {
      if (!foo (p, p1))
        return 0;
    }

  return 1;
}
/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue" } } */
