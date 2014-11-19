/* PR sanitizer/63690 */
/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

void
foo (void)
{
  (*"c")++;
}
