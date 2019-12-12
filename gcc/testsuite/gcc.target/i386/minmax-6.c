/* { dg-do compile } */
/* { dg-options "-O2 -march=haswell -mno-stackrealign" } */

unsigned short
UMVLine16Y_11 (short unsigned int * Pic, int y, int width)
{
  if (y != width)
    {
      y = y < 0 ? 0 : y;
      return Pic[y * width];
    }
  return Pic[y];
} 

/* We do not want the RA to spill %esi for it's dual-use but using
   pmaxsd is OK.  */
/* { dg-final { scan-assembler-not "rsp" { target { ! { ia32 } } } } } */
/* { dg-final { scan-assembler "pmaxsd" } } */
