/* { dg-do compile } */
/* { dg-additional-options "-funswitch-loops" } */

long t;
long a() {
  long b = t, c = t;
  for (; b < 31; b++)
    c <<= 1;
  return c;
}
long t1;
static
int d() {
  if (!t1)
    return 0;
e:
f:
  for (; a();)
    ;
  goto f;
  return 0;
}
int main() { d(); }
