/* PR tree-optimization/119417 */
/* { dg-do run { target int32 } } */

__attribute__((noipa)) void
foo (unsigned long long x)
{
  if (x != 0)
    __builtin_abort ();
}

unsigned v = 0x10000;

int
main ()
{
  unsigned long long a = 0;
  while (1)
    {
      a = a + ((v & 0xFFFF) * 2);
      foo (a);
      if (v)
	break;
    }
}
