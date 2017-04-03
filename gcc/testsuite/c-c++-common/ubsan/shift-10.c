/* PR sanitizer/80067 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=shift" } */

extern signed char a;
void
foo ()
{
  0 << ((647 > a) - 1);
}
