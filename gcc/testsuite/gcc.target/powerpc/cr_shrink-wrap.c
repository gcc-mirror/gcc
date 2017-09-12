/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue" } */

void foo(int i)
{
  if (i > 0)
    /* Non-volatile CR kill on true path should not prevent shrink-wrap.  */
    asm ("" : : : "cr2", "cr3");
}

/* { dg-final { scan-rtl-dump-times "Performing shrink-wrapping" 1 "pro_and_epilogue" } } */
