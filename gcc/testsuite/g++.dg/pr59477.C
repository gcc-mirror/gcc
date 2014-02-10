/* { dg-do compile } */
/* { dg-options "-O" } */

struct A
{
  unsigned *a, b;
  A (unsigned x) : a (), b (x) {}
};

struct B
{
  B (int);
  B (const B &) {}
};

B bar (B, B, A);
int v;

void
foo ()
{
  B c = 0;
  bar (c, c, A (1ULL << v));
}
