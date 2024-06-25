/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue" } */

/* Verify there is an early return without the prolog and shrink-wrap
   the function. */

int f (int);
int
advance (int dz)
{
  if (dz > 0)
    return (dz + dz) * dz;
  else
    return dz * f (dz);
}

/* { dg-final { scan-rtl-dump-times "Performing shrink-wrapping" 1 "pro_and_epilogue" } } */
