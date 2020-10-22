/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -fdump-rtl-pro_and_epilogue -mno-pcrel" } */

/* Verify we move the prologue past the TOC reference of 'j' and shrink-wrap
   the function. */
void bar();
int j;
void foo(int i)
{
  j = i;
  if (i > 0)
    {
      bar();
    }
}

/* { dg-final { scan-rtl-dump-times "Performing shrink-wrapping" 1 "pro_and_epilogue" } } */
