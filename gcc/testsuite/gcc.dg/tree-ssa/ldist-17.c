/* { dg-do compile } */
/* { dg-options "-O2 -ftree-loop-distribution -ftree-loop-distribute-patterns -fdump-tree-ldist-details" } */

typedef int mad_fixed_t;
struct mad_pcm
{
  unsigned int samplerate;
  unsigned short channels;
  unsigned short length;
  mad_fixed_t samples[2][1152];
};
struct mad_synth
{
  mad_fixed_t filter[2][2][2][16][8];
  unsigned int phase;
  struct mad_pcm pcm;
};
void mad_synth_mute (struct mad_synth *synth);
void
mad_synth_mute (struct mad_synth *synth)
{
  unsigned int ch;
  unsigned int s;
  unsigned int v;

  ch = 0U;
  while (ch < 2U)
    {
      s = 0U;
      while (s < 16U)
	{
	  v = 0U;
	  while (v < 8U)
	    {
	      synth->filter[ch][1][1][s][v] = 0;
	      synth->filter[ch][1][0][s][v] = 0;
	      synth->filter[ch][0][1][s][v] = 0;
	      synth->filter[ch][0][0][s][v] = 0;
	      v++;
	    }
	  s++;
	}
      ch++;
    }
  return;
}

/* { dg-final { scan-tree-dump "distributed: split to 4" "ldist" } } */
/* { dg-final { scan-tree-dump-times "generated memset zero" 4 "ldist" } } */
/* { dg-final { cleanup-tree-dump "ldist" } } */
