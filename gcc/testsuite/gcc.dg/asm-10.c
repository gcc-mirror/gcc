/* PR inline-asm/67448 */
/* { dg-do compile } */
/* { dg-options "" } */

void
f (int i)
{
  asm ("" : : "m"(i += 1)); /* { dg-error "not directly addressable" } */
  asm ("" : : "m"(i++)); /* { dg-error "not directly addressable" } */
  asm ("" : : "m"(++i)); /* { dg-error "not directly addressable" } */
  asm ("" : : "m"(i = 0)); /* { dg-error "not directly addressable" } */
}
