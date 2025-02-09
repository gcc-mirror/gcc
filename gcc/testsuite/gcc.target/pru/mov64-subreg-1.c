/* { dg-do assemble } */
/* { dg-options "-Os" } */
/* { dg-final { object-size text == 8 } } */


unsigned test(char a, unsigned long long b)
{
        return b;
}
