// { dg-do compile }
// { dg-additional-options "-march=core-avx2" { target x86_64-*-* i?86-*-* } }
struct A {
    int *elem[1];
};
int a, d, e;
A *b;
int *c;
int main()
{
  int *f = 0;
  for (; e; e++)
    if (b->elem[e])
      f = c;
  if (f)
    a = d;
}
