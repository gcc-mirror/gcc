/* { dg-do run } */

/* common */
char c;
/* arrays must be 8 byte aligned, regardless of size */
char c_ary[1];

/* data */
char d = 1;
char d_ary[1] = {1};

int main ()
{
  if (((unsigned long)&c_ary[0] & 7) != 0)
    return 1;
  if (((unsigned long)&d_ary[0] & 7) != 0)
    return 1;
  return 0;
}
