/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer -fdump-rtl-pro_and_epilogue" } */

/* Keep the epilogue stack tie without a frame pointer.  */

int
foo (int x)
{
  volatile int data[8];
  data[0] = x;
  return data[0];
}

/* { dg-final { scan-rtl-dump "UNSPEC_TIE" "pro_and_epilogue" } } */
