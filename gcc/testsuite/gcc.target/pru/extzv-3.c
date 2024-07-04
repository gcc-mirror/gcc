/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 12 } } */

struct S {
    unsigned int a : 9;
    unsigned int b : 4;
};

unsigned int test(struct S s)
{
  return s.b;
}
