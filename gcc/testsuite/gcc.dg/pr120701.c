/* { dg-do compile } */
/* { dg-options "-O2" } */

int a, b, c, e, f;
int main() {
  int d, g, i;
j:
  if (d >= 0)
    goto k;
  if (g >= 0)
    goto l;
k:
  i = a + 3;
m:
  f = 652685095 + 818172564 * g;
  if (-1101344938 * f - 1654872807 * d >= 0)
    goto n;
  goto l;
o:
  if (i) {
    c = -b;
    if (-c >= 0)
      goto l;
    g = b;
    b = i + 5;
    if (b * c)
      goto n;
    goto o;
  }
  if (e)
    goto m;
  goto j;
n:
  d = 978208086 * g - 1963072513;
  if (d + i)
    return 0;
  goto k;
l:
  goto o;
}
