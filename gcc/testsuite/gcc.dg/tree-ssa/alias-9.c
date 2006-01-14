/* { dg-do run } */
/* { dg-options "-O2" } */

void abort(void);
int main()
{
  int a[32];
  int i = 1;
  a[0] = 1;
  a[1] = 2;
  if (*(a + i) != 2)
    abort();
  return 0;
}

