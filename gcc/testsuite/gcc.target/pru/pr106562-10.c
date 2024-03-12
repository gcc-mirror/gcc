/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 16 } } */

int test(long long a)
{
        return a == 0;
}
