/* { dg-do compile } */
/* { dg-options "-O3 -fdump-rtl-ira -fdump-rtl-pro_and_epilogue"  } */

int __attribute__((noinline, noclone))
foo (int a)
{
  return a + 5;
}

static int g;

int __attribute__((noinline, noclone))
bar (int a)
{
  int r;

  if (a)
    {
      r = foo (a);
      g = r + a;
    }
  else
    r = a+1;
  return r;
}

/* { dg-final { scan-rtl-dump "Will split live ranges of parameters" "ira"  } } */
/* { dg-final { scan-rtl-dump "Split live-range of register" "ira"  } } */
/* { dg-final { scan-rtl-dump "Performing shrink-wrapping" "pro_and_epilogue"  } } */
/* { dg-final { cleanup-rtl-dump "ira" } } */
/* { dg-final { cleanup-rtl-dump "pro_and_epilogue" } } */
