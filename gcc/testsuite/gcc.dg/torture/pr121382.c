/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int a, b, c;
__attribute__((noipa))
static void d(int e, int f)
{
  if (e != 5 && e != 2147483647)
    __builtin_abort();
  f = 2147483647;
  do {
    f = f - e;
    if (c - 9 * f) {
      __builtin_abort();
    }
  } while (c);
}
__attribute__((noipa))
int main(void)
{
  d(2147483647, 2147483647);
  return 0;
}
