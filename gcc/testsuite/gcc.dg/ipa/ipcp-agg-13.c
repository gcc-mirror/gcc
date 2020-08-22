/* { dg-do compile } */
/* { dg-options "-O3 -fno-ipa-sra -fdump-ipa-cp --param ipa-cp-eval-threshold=1" } */

int data;
int fn();

int __attribute__((noinline)) f1 (int *p)
{
  data = *p;
  fn ();
  return 0;
} 

int __attribute__((noinline)) f2 (int *p)
{
  *p = *p + 1;
  f1 (p);
  return 1;
}

int __attribute__((noinline)) f3 (int a, int *p)
{
  *p = a - 2;
  f1 (p);
  return 1;
}

int f4 ()
{
  int i;

  for (i = 0; i < 100; i++)
    {
       int v = 2;

       f2 (&v);
       f3 (6, &v);
    }

  return 0;
}

/* { dg-final { scan-ipa-dump "Aggregate replacements: 0\\\[0]=2" "cp" } } */ 
/* { dg-final { scan-ipa-dump "Aggregate replacements: 0\\\[0]=3" "cp" } } */ 
/* { dg-final { scan-ipa-dump "Aggregate replacements: 0\\\[0]=4" "cp" } } */ 
