/* { dg-do compile } */
/* { dg-options "-O2 -mdejagnu-cpu=power10 -mrop-protect -fdump-rtl-pro_and_epilogue" } */
/* { dg-require-effective-target rop_ok } Only enable on supported ABIs. */

/* Verify we still attempt shrink-wrapping when using -mrop-protect
   and there are no function calls.  */

long
foo (long arg)
{
  if (arg)
    asm ("" ::: "r20");
  return 0;
}

/* { dg-final { scan-rtl-dump-times "Performing shrink-wrapping" 1 "pro_and_epilogue" } } */
