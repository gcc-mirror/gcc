/* { dg-do compile } */
/* { dg-options "-O2 -march=haswell -mno-stackrealign" } */

extern int numBins;
extern int binOffst;
extern int binWidth;
extern int Trybin;
void foo (int);

void bar (int aleft, int axcenter)
{
  int a1LoBin = (((Trybin=((axcenter + aleft)-binOffst)/binWidth)<0)
		 ? 0 : ((Trybin>numBins) ? numBins : Trybin));
  foo (a1LoBin);
}

/* We do not want the RA to spill %esi for it's dual-use but using
   pminsd is OK.  */
/* { dg-final { scan-assembler-not "rsp" { target { ! { ia32 } } } } } */
/* { dg-final { scan-assembler "pminsd" } } */
