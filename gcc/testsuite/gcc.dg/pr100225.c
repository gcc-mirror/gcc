/* PR rtl-optimization/100225 */
/* { dg-do compile } */
/* { dg-options "-O1 -fmodulo-sched" } */
/* { dg-require-effective-target alloca } */

void
vorbis_synthesis_lapout (void);

void
ov_info (int **lappcm, int ov_info_i)
{
  while (ov_info_i < 1)
    lappcm[ov_info_i++] = __builtin_alloca (1);

  vorbis_synthesis_lapout ();
}
