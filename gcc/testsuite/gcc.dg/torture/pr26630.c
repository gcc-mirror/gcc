/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

extern void abort(void);
int main()
{
  int a1 = 40000;
  int c1 = ( ((int)(short)(a1-10000)) + 10000)*2;
  if (c1 != 80000)
    abort();
  return 0;
}
