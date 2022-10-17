/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 16 } } */


long long test(int a)
{
        return a;
}
