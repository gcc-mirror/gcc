/* { dg-do run } */

int a = 1, b, c = 2, d;
int main() {
  if (b)
    goto L2;
 L1:
  {
    int e = c;
    a = 1 % a;
    while (e && 1 <= d)
      ;
    d >= b;
  L2:
    if (1 >= e)
      goto L1;
  }
  return 0;
}
