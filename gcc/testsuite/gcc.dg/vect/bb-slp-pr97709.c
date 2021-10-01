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
  goto *(void*)(__INTPTR_TYPE__)a;
g:
  d = 0;
h:
  c = 1;
  goto *(void*)(__INTPTR_TYPE__)a;
i:
  {
    struct b b = {c, d};
    k(b);
  }
}
