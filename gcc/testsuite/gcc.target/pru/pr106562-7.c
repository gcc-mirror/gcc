/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 8 } } */

int test(unsigned a)
{
        return a >= 1;
}
