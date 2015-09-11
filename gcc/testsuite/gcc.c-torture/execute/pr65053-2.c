/* PR tree-optimization/65053 */

int i;
unsigned int x;

int
main ()
{
  asm volatile ("" : "+g" (x));
  unsigned int n = x;
  unsigned int u = 32;
  if (n >= 32)
    __builtin_abort ();
  if (n != 0)
    u = n + 32;

  while (u != 32)
    {
      asm ("" : : "g" (u));
      u = 32;
      i = 1;
    }

  if (i)
    __builtin_abort ();
  return 0;
}
