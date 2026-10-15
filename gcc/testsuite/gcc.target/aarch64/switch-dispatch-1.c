/* PR middle-end/126236 */
/* { dg-do assemble } */
/* { dg-options "-O2 -save-temps" } */

/* Each of the 11 continuing handlers, the default handler and the loop entry
   get a dispatch of their own, so 13 register indirect branches.  All of them
   share the one table, whose entries are relative to a single base label,
   which must be defined exactly once.  */

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

/* { dg-final { scan-assembler-times "\\tbr\\tx" 13 } } */
/* { dg-final { scan-assembler-times "\\.Lrtx\[0-9\]+:" 1 } } */

/* A copy can sit far from that one base label, further than ADR reaches, so the
   copies materialise it with ADRP and ADD.  */
/* { dg-final { scan-assembler-times "adrp\\tx\[0-9\]+, \\.Lrtx" 13 } } */
/* { dg-final { scan-assembler-times "add\\tx\[0-9\]+, x\[0-9\]+, :lo12:\\.Lrtx" 13 } } */
/* { dg-final { scan-assembler-not "\\tadr\\tx\[0-9\]+, \\.Lrtx" } } */
