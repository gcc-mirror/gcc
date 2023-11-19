/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 24 } } */

struct S {
    unsigned int a : 5;
    unsigned int b : 1;
    unsigned int c : 1;
};

void test(struct S *s, unsigned int val)
{
  s->b = val;
}
