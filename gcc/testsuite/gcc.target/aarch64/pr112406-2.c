/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-march=armv8-a+sve -Ofast" } */

double MADPictureC1;
extern int PictureRejected[];
int PictureMAD_0, MADModelEstimator_n_windowSize_i, MADModelEstimator_n_windowSize_oneSampleQ;

void MADModelEstimator_n_windowSize() {
  int estimateX2 = 0;
  for (; MADModelEstimator_n_windowSize_i; MADModelEstimator_n_windowSize_i++) {
    if (MADModelEstimator_n_windowSize_oneSampleQ &&
        !PictureRejected[MADModelEstimator_n_windowSize_i])
      estimateX2 = 1;
    if (!PictureRejected[MADModelEstimator_n_windowSize_i])
      MADPictureC1 += PictureMAD_0;
  }
  if (estimateX2)
    for (;;)
      ;
}
