/* { dg-do compile } */
/* { dg-additional-options "-funswitch-loops" } */

int save, mask_nbits;

void execute(long imm)
{
  long shift = 0;
  int destReg[4];
  for (unsigned i = 0; i < 4; i++)
    {
      if (imm)
	shift = 1ULL << mask_nbits;
      destReg[i] = shift;
      save = destReg[0];
    }
}
