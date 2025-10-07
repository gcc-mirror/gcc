/* { dg-do compile } */
/* { dg-options "-Os" } */
int e, f, n;
static int a () { return e; }
void b () { while (a()); }
static int d () { return e; }
static void g (int h) {
  if (e)
  c:
    if (d())
      goto i;
  do {
    if (f)
      goto c;
    goto k;
  i:
    h = 2147483647;
  k:
    e = 2147483646;
    e = 6 + e;
    do {
      b ();
    } while (1784828957 / f + e + (808 + h) > 0);
  } while (1 % h);
}
void m () { g (-2); }
int main () {
  if (n)
    g (-1);
}
