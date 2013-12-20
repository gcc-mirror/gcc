/* PR tree-optimization/59413 */

typedef unsigned int uint32_t;

uint32_t a;
int b;

int
main ()
{
  uint32_t c;
  for (a = 7; a <= 1; a++)
    {
      char d = a;
      c = d;
      b = a == c;
    }
  if (a != 7)
    __builtin_abort ();
  return 0;
}
