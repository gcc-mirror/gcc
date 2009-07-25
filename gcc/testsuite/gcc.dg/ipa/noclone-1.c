/* { dg-do compile } */
/* { dg-options "-O3 -fipa-cp -fipa-cp-clone -fdump-ipa-cp -fno-early-inlining"  } */

int global_1, global_2;

__attribute__((__noclone__)) int g (int b, int c)
 {
  global_1 = b;
  global_2 = c;
}

__attribute__((__noclone__)) int f (int a)
{
  /* Second parameter of g gets different values.  */
  if (a > 0)
    g (a, 3);
  else
    g (a, 5);
}

int main ()
{
  f (7);
  return 0;
}


/* { dg-final { scan-ipa-dump-times "versioned function" 0 "cp"  } } */
/* { dg-final { cleanup-ipa-dump "cp" } } */
