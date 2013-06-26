/* PR rtl-optimization/57559  */

/* { dg-do compile } */
/* { dg-options "-march=z10 -m64 -mzarch  -O1" } */

typedef int int32_t;
typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
struct _IO_marker
{
};
static const int32_t mfcone = 1;
static const uint8_t *mfctop = (const uint8_t *) &mfcone;
int32_t
decContextTestEndian (uint8_t quiet)
{
  int32_t res = 0;
  uint32_t dle = (uint32_t) 0;
  if (*(int *) 10 != 0)
    {
      res = (int32_t) * mfctop - dle;
    }
  return res;
}
