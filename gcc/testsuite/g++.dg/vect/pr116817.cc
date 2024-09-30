/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

int main_ulData0;
unsigned *main_pSrcBuffer;
int main(void) {
  int iSrc = 0;
  bool bData0;
  for (; iSrc < 4; iSrc++) {
    if (bData0)
      main_pSrcBuffer[iSrc] = main_ulData0;
    else
      main_pSrcBuffer[iSrc] = 0;
    bData0 = !bData0;
  }
}
