/* { dg-do compile { target bitint } } */
/* { dg-options "-O3 -march=armv8-a" } */
/* { dg-additional-options "-mmax-vectorization --param=vect-epilogues-nomask=0 -fdump-tree-vect-details" } */

typedef __INT16_TYPE__ int16_t;
typedef __INT32_TYPE__ int32_t;
typedef unsigned _BitInt(17) uint17_t;

#define SAT_VALUE(X) ((int16_t) (-((int16_t) ((X) < 0)) ^ 32767))

__attribute__((noipa))
void
narrow_range (int16_t *__restrict out, const int32_t *__restrict in, int n)
{
  for (int i = 0; i < n; ++i)
    {
      int32_t source = in[i];
      uint17_t range = (uint17_t) source + (uint17_t) 32768;
      out[i] = (range > (uint17_t) 65535
		? SAT_VALUE (source) : (int16_t) source);
    }
}

/* { dg-final { scan-tree-dump-not "sat_trunc pattern recognized" "vect" } } */
