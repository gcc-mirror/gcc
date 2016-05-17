/* { dg-do compile } */
/* { dg-options "-O2 -fsplit-paths -fdump-tree-dom3-details" } */

/* There should be two references to bufferstep that turn into
   constants.  */
/* { dg-final { scan-tree-dump-times "Replaced .bufferstep_\[0-9\]+. with constant .0." 1 "dom3"} } */
/* { dg-final { scan-tree-dump-times "Replaced .bufferstep_\[0-9\]+. with constant .1." 1 "dom3"} } */

/* And some assignments ought to fold down to constants.  */
/* { dg-final { scan-tree-dump-times "Folded to: _\[0-9\]+ = 1;" 1 "dom3"} } */
/* { dg-final { scan-tree-dump-times "Folded to: _\[0-9\]+ = 0;" 1 "dom3"} } */

/* The XOR operations should have been optimized to constants.  */
/* { dg-final { scan-tree-dump-not "bit_xor" "dom3"} } */


extern int *stepsizeTable;

void
adpcm_coder (signed char *outdata, int len)
{
  signed char *outp;
  int delta;
  int outputbuffer;
  int bufferstep = 0;
  outp = (signed char *) outdata;
  int step = 0;
  int index = 0;
  int diff = 0;
  for (; len > 0; len--)
    {
      delta = 0;
      if (diff >= step)
	delta = 4;
      step = stepsizeTable[index];
      if (bufferstep)
	outputbuffer = (delta << 4) & 0xf0;
      else
	*outp++ = (delta & 0x0f) | outputbuffer;
      bufferstep = !bufferstep;
    }
}
