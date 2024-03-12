/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 12 } } */

int test(unsigned long long a)
{
        return a > 0;
}
