/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text <= 12 } } */

int test(int a)
{
        return a == 0;
}
