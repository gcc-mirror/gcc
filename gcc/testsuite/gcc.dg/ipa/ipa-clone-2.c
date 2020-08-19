/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details -fno-early-inlining --param ipa-cp-max-recursive-depth=8 --param=ipa-cp-eval-threshold=400" } */

int fn();

int data[100];

int recur_fn (int i)
{
  int j;
   
  if (i == 6)
    {
      fn();
      fn();
      fn();
      fn();
      fn();
      fn();
      fn();
      fn();
      fn();
      fn();
      fn();
      fn();
      return 10;
    }

  data[i] = i; 

  for (j = 0; j < 100; j++)
    recur_fn (i + 1);

  return i; 
}

int main ()
{
  int i;

  for (i = 0; i < 100; i++)
    recur_fn (1) + recur_fn (-5);

  return 1;
}

/* { dg-final { scan-ipa-dump-times "Creating a specialized node of recur_fn/\[0-9\]*\\." 12 "cp" } } */
