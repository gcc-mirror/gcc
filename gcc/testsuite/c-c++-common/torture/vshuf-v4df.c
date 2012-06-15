#if __SIZEOF_DOUBLE__ == 8 && __SIZEOF_LONG_LONG__ == 8
typedef double V __attribute__((vector_size(32)));
typedef unsigned long long VI __attribute__((vector_size(32)));
#else
#define UNSUPPORTED
#endif

#define A	0.69314718055994530942
#define B	2.7182818284590452354
#define C	2.30258509299404568402
#define D	1.4426950408889634074

#define W	0.31830988618379067154
#define X	3.14159265358979323846
#define Y	1.41421356237309504880
#define Z	0.70710678118654752440

#include "vshuf-4.inc"
#include "vshuf-main.inc"
