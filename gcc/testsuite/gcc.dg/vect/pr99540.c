/* { dg-do compile } */
/* { dg-additional-options "-ftrapv -ffloat-store -march=armv8.2-a+sve" { target aarch64*-*-* } } */

float *MSalign2m2m_rec_initverticalw, *MSalign2m2m_rec_currentw;

void
match_ribosum (int MSalign2m2m_rec_i, int MSalign2m2m_rec_lgth1,
               int MSalign2m2m_rec_lgth2)
{
  float **WMMTX;

  while (MSalign2m2m_rec_i < 1)
    WMMTX[MSalign2m2m_rec_i++][0] = MSalign2m2m_rec_initverticalw[0];

  while (MSalign2m2m_rec_i < MSalign2m2m_rec_lgth1)
    MSalign2m2m_rec_initverticalw[MSalign2m2m_rec_i++] += 0.1;

  while (MSalign2m2m_rec_i < MSalign2m2m_rec_lgth2)
    MSalign2m2m_rec_currentw[MSalign2m2m_rec_i++] += 0.1;
}
