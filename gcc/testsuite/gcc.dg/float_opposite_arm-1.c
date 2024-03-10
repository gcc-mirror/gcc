/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-original -fdump-tree-optimized" } */
/* { dg-add-options ieee } */
/* PR middle-end/95351 */

int Foo(double possiblyNAN, double b, double c)
{
    return (possiblyNAN <= 2.0) || ((possiblyNAN  > 2.0) && (b > c));
}

/* Make sure we don't remove either >/<=  */

/* { dg-final { scan-tree-dump "possiblyNAN > 2.0e.0" "original" } } */
/* { dg-final { scan-tree-dump "possiblyNAN_\[0-9\]+.D. > 2.0e.0" "optimized" } } */

/* { dg-final { scan-tree-dump "possiblyNAN <= 2.0e.0" "original" } } */
/* { dg-final { scan-tree-dump "possiblyNAN_\[0-9\]+.D. <= 2.0e.0" "optimized" } } */
