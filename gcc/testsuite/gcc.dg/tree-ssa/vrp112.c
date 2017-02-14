/* PR tree-optimization/78819 */
/* { dg-do run } */
/* { dg-options "-O2" } */

__attribute__((noinline, noclone)) void
foo (int argc)
{
  if (argc <= 0 || argc > 3)
    return;

  switch (argc)
    {
    case 1:
    case 3:
      if (argc != 3)
	__builtin_abort ();
      break;
    case 2:
      asm ("");
      break;
    default:
      __builtin_abort ();
    }
}

int
main (void)
{
  foo (3);
  return 0;
}
