/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <math.h>

int Hann_i, PsyBufferUpdate_psyInfo_0, PsyBufferUpdate_i;
double *mdct_data;
double PsyBufferUpdate_sfreq;
void PsyBufferUpdate() {
  if (PsyBufferUpdate_psyInfo_0 == 4)
    for (; Hann_i;)
      ;
  {
    double xr_0 = cos(PsyBufferUpdate_psyInfo_0);
    PsyBufferUpdate_sfreq = sin(PsyBufferUpdate_psyInfo_0);
    for (; PsyBufferUpdate_psyInfo_0; PsyBufferUpdate_i++)
      mdct_data[PsyBufferUpdate_i] = xr_0 * PsyBufferUpdate_sfreq;
  }
}

