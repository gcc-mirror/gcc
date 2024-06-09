/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 28 } } */

struct S {
    unsigned int a : 3;
    unsigned int b : 3;
    unsigned int c : 3;
};

void test(struct S *s, unsigned int val)
{
  s->b = val;
}
