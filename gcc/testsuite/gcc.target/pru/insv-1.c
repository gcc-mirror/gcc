/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 16 } } */

struct S {
    unsigned int a : 5;
    unsigned int b : 1;
    unsigned int c : 1;
};

void test(struct S *s)
{
  s->b = 1;
}
