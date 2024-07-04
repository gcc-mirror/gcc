/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

#define X 1100000000
unsigned char a;
long b = X;
int c[9][1];
unsigned d;
static long *e = &b, *f = &b;
int g() {
  if (a && a <= '9')
    return '0';
  if (a)
    return 10;
  return -1;
}
int main() {
  d = 0;
  for (; (int)*f -(X-1) + d < 9; d++)
    c[g() + (int)*f + ((int)*e - X) -(X-1) + d]
     [0] = 0;
}
