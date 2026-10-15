/* PR middle-end/126236 */
/* { dg-do compile } */
/* { dg-options "-O2 -fduplicate-switch-dispatch -mbranch-protection=bti" } */

/* Duplicating the dispatch must not duplicate the landing pads: one BTI J per
   case handler, not one per handler per dispatch copy.  */

void
interp (const unsigned char *pc, volatile unsigned *state)
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

/* 12 table targets plus the function entry.  */
/* { dg-final { scan-assembler-times "bti\\tj|hint\\t36" 12 } } */
/* { dg-final { scan-assembler-times "bti\\tc|hint\\t34" 1 } } */
