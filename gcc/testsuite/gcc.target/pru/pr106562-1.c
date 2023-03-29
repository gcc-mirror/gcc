/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 40 } } */


char test(unsigned long long a, unsigned long long b)
{
        return a && b;
}
