/* PR middle-end/126236 */
/* { dg-do run } */
/* { dg-additional-options "-fduplicate-switch-dispatch --param=case-values-threshold=1" } */

/* Every predecessor of the dispatch chain can take a copy, so the original
   dispatch block and its shared table's former owner become unreachable.  */

volatile unsigned int state[3];

__attribute__((noipa)) void
interp (const unsigned char *pc)
{
  goto first;
  for (;;)
    switch (*pc++)
      {
      case 0: return;
      case 1: first: state[0] += 1; break;
      case 2: state[1] += 2; break;
      default: state[2] ^= 3; break;
      }
}

int
main (void)
{
  static const unsigned char prog[] = { 2, 3, 1, 0 };

  interp (prog);
  if (state[0] != 2 || state[1] != 2 || state[2] != 3)
    __builtin_abort ();
  return 0;
}
