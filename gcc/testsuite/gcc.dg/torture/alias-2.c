/* { dg-do run } */

/* We do not want to treat int[3] as an object that cannot overlap
   itself but treat it as arbitrary sub-array of a larger array object.  */
int ar1(int (*p)[3], int (*q)[3])
{
  (*p)[0] = 1;
  (*q)[1] = 2;
  return (*p)[0];
}
int main()
{
  int a[4];
  if (ar1 ((int (*)[3])&a[1], (int (*)[3])&a[0]) != 2)
    __builtin_abort ();
  return 0;
}
