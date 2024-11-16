/* { dg-do run } */

/* PR middle-end/117426 */


/* o and q stack variables should not be allocated at the same parition.
   At -O3 with unrolling, the stack conflicts code was missing both variables
   were alive at the same time.  */
__attribute__((noipa)) void func1() {}
int a[6];
int b, d, i, j, l, m, n;
char *c;
int f[8][8][4];
int *g = &d;
char p[11];
int main() {
  short q[6];
  int k = 0;
  for (; k < 2; k++) {
    {
      char o[3];
      int e = 53;
      char *h = o;
      c = p;
      while (e)
        *c++ = e /= 10;
      while (c != p)
        *h++ = *--c;
      *h++ = '\0';
      n = h - o;
    }
    q[n - 2] = 1;
  }
  *g = q[1];
  if (d != 1)
    __builtin_abort ();
  l = 0;
  for (; l < 10; l++)
    if (m)
      func1();
  i = 0;
  for (; i < 7; i++) {
    j = 0;
    for (; j < 7; j++)
      b = a[b];
  }
  j = 0;
  for (; j < 8; j++) {
    l = 0;
    for (; l < 4; l++)
      b = a[b ^ f[i][j][l]];
  }
}
