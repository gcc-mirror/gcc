/* PR middle-end/126236 */
/* { dg-do run } */
/* { dg-additional-options "-fduplicate-switch-dispatch" } */

/* A bytecode interpreter whose switch sits at the head of the execution
   loop.  Every opcode, the default and an out of range index are executed,
   so the range check has to survive being copied into each handler.  */

unsigned int state[16];

__attribute__((noipa)) void
interp (const unsigned char *pc)
{
  for (;;)
    switch (*pc++)
      {
      case 0: return;
      case 1: state[0] += 1; break;
      case 2: state[1] ^= 2; break;
      case 3: state[2] -= 3; break;
      case 4: state[3] |= 4; break;
      case 5: state[4] &= 5; break;
      case 6: state[5] += 6; break;
      case 7: state[6] ^= 7; break;
      case 8: state[7] -= 8; break;
      case 9: state[8] |= 9; break;
      case 10: state[9] &= 10; break;
      case 11: state[10] += 11; break;
      default: state[11] ^= 12; break;
      }
}

static const unsigned char prog[] = {
  1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,	/* every explicit case */
  12, 200, 255,				/* the default, in and out of range */
  1, 1, 1, 11, 11,			/* repeats */
  0
};

int
main (void)
{
  unsigned int expect[16] = { 0 };

  for (unsigned i = 0; i < 16; i++)
    state[i] = i + 1;
  for (unsigned i = 0; i < 16; i++)
    expect[i] = i + 1;

  expect[0] += 1; expect[1] ^= 2; expect[2] -= 3; expect[3] |= 4;
  expect[4] &= 5; expect[5] += 6; expect[6] ^= 7; expect[7] -= 8;
  expect[8] |= 9; expect[9] &= 10; expect[10] += 11;
  expect[11] ^= 12; expect[11] ^= 12; expect[11] ^= 12;
  expect[0] += 1; expect[0] += 1; expect[0] += 1;
  expect[10] += 11; expect[10] += 11;

  interp (prog);

  for (unsigned i = 0; i < 16; i++)
    if (state[i] != expect[i])
      __builtin_abort ();
  return 0;
}
