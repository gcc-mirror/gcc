/* PR debug/101266 */
/* { dg-do compile } */
/* { dg-options "-g -O2" } */

void
foo (int (*p)[(int){1}])
{
}
