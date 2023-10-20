/* { dg-do compile } */
/* { dg-options "-mno-sse" } */
/* PR c/111903 */

int i;
_Float16 f;
int bar(...);
void
foo (void)
{
  i /= bar ((_Complex _Float16) f); /* { dg-error "" } */
}
