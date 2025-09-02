/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

int a;
int main()
{
  int c = -2147483647;
  int d = -2147483647;
  int e = 2147483647;
  if (0)
  f:
    e = d + e - 2;
g:
  if (d - c - e > 0) {
    a = -c;
    if (a + d) {
      d = 1;
      goto g;
    }
    return 0;
  }
  if (e)
    goto f;
  return 0;
}
