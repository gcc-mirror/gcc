/* PR target/37395 */

int
f (int j)
{
  int i;
  asm volatile ("" : "=r"(i));
  if (i >= 0)
    j = 0;
  return j;
}
