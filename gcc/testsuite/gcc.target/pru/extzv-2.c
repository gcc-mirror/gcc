/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 8 } } */

struct S {
    unsigned int a : 5;
    unsigned int b : 1;
    unsigned int c : 24;
    unsigned int d : 2;
};

unsigned int test(struct S s)
{
  return s.d;
}
