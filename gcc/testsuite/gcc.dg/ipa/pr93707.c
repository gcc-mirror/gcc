/* { dg-do compile } */
/* { dg-options "-O3 --param ipa-cp-eval-threshold=1 -fdump-ipa-cp" } */

int foo();
int data[100];

__attribute__((noinline)) static int recur_fn (int i, int j, int depth)
{
   if (depth > 10)
     return 1;

   data[i + j]++;

   if (depth & 3)
     recur_fn (i, 1, depth + 1);
   else
     recur_fn (i, j & 1, depth + 1);

   foo();

   return i + j;
}

int caller (int v, int depth)
{
  recur_fn (1, v, depth);

  return 0;
}

/* { dg-final { scan-ipa-dump-times "Clone of recur_fn/" 2 "cp" } } */
