/* { dg-do compile } */
/* { dg-options "-Wuninitialized" } */

struct xxx {
    short a;
    short b;
    void *c;
};

void bar(struct xxx);

void foo(struct xxx *p, int i)
{
  struct xxx s0 = *p;
  struct xxx s = s0;
  if (s.a) i++;
  bar(s);
}
