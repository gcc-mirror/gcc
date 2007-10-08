/* { dg-do compile } */

/* We used to ICE with type-checking enabled.  */

unsigned int bfstages(int M, float *Utbl, int Ustride)
{
   return ((unsigned int) 1 << M) * Ustride;
}
