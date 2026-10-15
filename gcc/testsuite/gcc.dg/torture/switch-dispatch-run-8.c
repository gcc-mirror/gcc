/* PR middle-end/126236 */
/* { dg-do run } */
/* { dg-additional-options "-fduplicate-switch-dispatch --param=max-goto-duplication-insns=128 --param=case-values-threshold=1" } */

/* The block with the asm goto falls through to the range check of the
   switch, and the asm goto names that block as well.  Such a block must
   not become part of a copy of the dispatch.  */

volatile unsigned int sd_count[12];

__attribute__ ((noipa)) void
sd_interp (const unsigned char *pc)
{
  sd_count[0] = 0;
  for (;;)
    {
      unsigned int op = *pc++;
      asm goto ("" :::: next);
    next:
      switch (op)
	{
	case 0: return;
	case 1: sd_count[1] += 1; break;
	case 2: sd_count[2] += 2; break;
	case 3: sd_count[3] += 3; break;
	case 4: sd_count[4] += 4; break;
	case 5: sd_count[5] += 5; break;
	case 6: sd_count[6] += 6; break;
	case 7: sd_count[7] += 7; break;
	case 8: sd_count[8] += 8; break;
	case 9: sd_count[9] += 9; break;
	case 10: sd_count[10] += 10; break;
	default: sd_count[11] += 11; break;
	}
    }
}

int
main (void)
{
  static const unsigned char prog[] = { 1, 2, 3, 200, 10, 0 };
  sd_interp (prog);
  if (sd_count[1] != 1 || sd_count[2] != 2 || sd_count[3] != 3
      || sd_count[10] != 10 || sd_count[11] != 11)
    __builtin_abort ();
  return 0;
}
