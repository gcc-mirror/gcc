/* { dg-do compile } */
/* { dg-options "-std=c99 -O3 -fPIC" } */
long ZDICT_fillNoise_p, ZDICT_trainFromBuffer_legacy_result;
unsigned ZDICT_fillNoise_acc;
int ZDICT_totalSampleSize_nbFiles;
static void ZDICT_fillNoise(void *buffer, long length) {
  unsigned prime2 = 9;
  for (ZDICT_fillNoise_p = 0; ZDICT_fillNoise_p < length; ZDICT_fillNoise_p++)
    ZDICT_fillNoise_acc *= ((char *)buffer)[ZDICT_fillNoise_p] = prime2;
}
long ZDICT_trainFromBuffer_legacy() {
  void *newBuff;
  long total = 0;
  for (; ZDICT_totalSampleSize_nbFiles;)
    total += 0;
  long sBuffSize = total;
  newBuff = 0;
  ZDICT_fillNoise(newBuff + sBuffSize, 32);
  return ZDICT_trainFromBuffer_legacy_result;
}
