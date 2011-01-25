/* { dg-do compile { target powerpc*-*-* } } */
/* { dg-options "-O3 -ftree-loop-linear" } */

unsigned char inUse[256];
unsigned char len[6][258];
int code[6][258];
unsigned int crc32Table[256] = { };
  unsigned int getGlobalCRC (void) { }
  int bsLive;
void bsW (int n, unsigned int v) {
 while (bsLive >= 8) {}
 }
 void hbAssignCodes (int * code,         unsigned char * length, int minLen,
int maxLen, int alphaSize) {
   int n, vec, i;
   for (n = minLen;n <= maxLen;n++)
       for (i = 0; i < alphaSize;i++)
      code[i] = vec;
   }
  void sendMTFValues (void) {
   int v, t, i, j, gs, ge, totc, bt, bc, iter;
   int nSelectors, alphaSize, minLen, maxLen, selCtr;
   int nGroups, nBytes;
 {
    while (1)
  {
  break;
  }
       hbAssignCodes (&code[t][0], &len[t][0], minLen, maxLen, alphaSize);
     unsigned char inUse16[16];
     for (i = 0;i < 16;i++)
 if (inUse16[i])
  {
      for (j = 0;j < 16;j++)
   if (inUse[i * 16 + j])    { }
    }
   }
   for (i = 0; i < nSelectors;i++)     { }
   for (t = 0; t < nGroups;t++)
 {
       int curr = len[t][0];
       for (i = 0; i < alphaSize;i++)
          while (curr < len[t][i])     { }
     }
   while (1)
       for (i = gs; i <= ge;i++)  { }
 }

