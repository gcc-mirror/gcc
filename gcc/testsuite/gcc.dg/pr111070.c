/* { dg-do compile } */
/* { dg-options "-O" } */

/* common */
char c;
/* arrays must be 8 byte aligned, regardless of size */
char c_ary[1];

/* data */
char d = 1;
char d_ary[1] = {1};

int main ()
{
  if (((__UINTPTR_TYPE__)&c_ary[0] & 7) != 0)
    return 1;
  if (((__UINTPTR_TYPE__)&d_ary[0] & 7) != 0)
    return 1;
  return 0;
}
