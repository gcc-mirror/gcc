/* PR c/104427 */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-add-options float16 } */
/* { dg-require-effective-target float16 } */

_Float16 x, y;

int
foo ()
{
  return __builtin_assoc_barrier (x + y) - y;
}
