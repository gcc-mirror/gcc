/* PR middle-end/111338 */
/* { dg-do compile { target bitint575 } } */
/* { dg-options "-O1" } */

_BitInt(575) e;

_BitInt(575)
foo (void)
{
  return e & 1;
}
