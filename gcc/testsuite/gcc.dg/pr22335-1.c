/* { dg-do compile } */
/* { dg-options "-O1 -fdelete-null-pointer-checks"  } */
int t(int *a)
{
  int i;
  *a = 1;
  i = a == 0;
  return i;
}

