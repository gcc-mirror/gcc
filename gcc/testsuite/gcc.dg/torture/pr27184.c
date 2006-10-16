/* Copyright 2006 Free Software Foundation */

/* Make sure a and a1 alias each other.
   Incomplete array types used to not be unified, which broke aliasing.  */

/* { dg-do run } */

typedef long atype[];
typedef long atype1[];
int NumSift (atype *a, atype1 *a1)
{
  (*a)[0] = 0;
  (*a1)[0] = 1;
  return (*a)[0];
}
int main(void)
{
  long a[2];
  if (!NumSift(&a, &a))
    __builtin_abort ();
  return 0;
}
