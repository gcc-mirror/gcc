/* PR inline-asm/85022 */

extern struct B b;

void
foo ()
{
  __asm ("" : "+m" (b));
}
