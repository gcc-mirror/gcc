/* { dg-do compile } */

int a;
struct b {
  int c;
  int d;
};
void k (struct b);
struct b
e()
{
  void *f[] = {&&g, &&h, &&i, &&j};
  int d, c;
j:
  goto *a;
g:
  d = 0;
h:
  c = 1;
  goto *a;
i:
  {
    struct b b = {c, d};
    k(b);
  }
}
