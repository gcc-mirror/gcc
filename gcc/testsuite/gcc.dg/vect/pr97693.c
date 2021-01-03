/* { dg-do compile } */

extern short a[];
int b;
short c, d;
unsigned e() {
  if (c)
    return c;
  return d;
}
void f() {
  for (unsigned g = b; g; g += 6)
    for (_Bool h = 0; h < (_Bool)e(); h = 1)
      a[g] = 1 / b;
}
