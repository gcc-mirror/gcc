/* PR middle-end/117375 ICE with -fdiagnostics-show-context=1 patch in sink pass.  */ 
/*    { dg-do compile }
      { dg-options "-O2 -Wall -fdiagnostics-show-context=1" } */

int st, st_0;
int nbFilledBytes, max;
void ec_enc_shrink();
void max_allowed() {
  int nbAvailableBytes = nbFilledBytes;
  if (st && st_0)
    if (max < nbAvailableBytes)
      ec_enc_shrink();
}
