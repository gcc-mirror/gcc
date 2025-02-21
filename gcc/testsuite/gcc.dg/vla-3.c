/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* This used to crash as we did not preserve the correct type
   for __SIZE_TYPE__. See PR22439. */

char foo(__SIZE_TYPE__ n)
{
    char c[1][n];
    return c[0][0];
}
