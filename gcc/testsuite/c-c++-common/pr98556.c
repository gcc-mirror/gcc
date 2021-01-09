/* PR c++/98556 */
/* { dg-do compile } */
/* { dg-options "-O0" } */

enum T { E = -__LONG_MAX__ - 1 };

enum T
foo (char *p, char *q)
{
  return (enum T) (p - q);
}
