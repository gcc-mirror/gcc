/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue" } */

/* Verify there is an early return without the prolog and shrink-wrap
   the function. */
void bar ();
long
foo (long i, long cond)
{
  if (cond)
    bar ();
  return i+1;
}

/* { dg-final { scan-rtl-dump-times "Performing shrink-wrapping" 1 "pro_and_epilogue" } } */
