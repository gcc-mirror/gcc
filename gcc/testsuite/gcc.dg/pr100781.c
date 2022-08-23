/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug  " } */

struct a {
  int b;
};
long c(short d, long e, struct a f) {
g:;
  int h = f.b <= e, i = d, n = h >= d;
  if (!n)
    goto j;
  goto k;
j:;
  long l = 5;
  if (l)
    goto m;
  d = 0;
m:
  if (d)
    return f.b;
k:
  goto g;
}
int main() { }

