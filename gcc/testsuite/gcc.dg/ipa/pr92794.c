/* { dg-do compile } */
/* { dg-options "-O3 --param ipa-cp-eval-threshold=1" } */

int data[100];
int depth = 0;

int recur_fn (int *__restrict p)
{
  int i = *p;

  if (depth++ > 6) 
    return 10;

  data[i] = i; 

  recur_fn (&i);

  depth--;

  return i;
}

int main ()
{
  int i = 1;

  recur_fn (&i);

  return 0;
}
