/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 32 } } */


char test(long long a)
{
        return a < 10;
}
