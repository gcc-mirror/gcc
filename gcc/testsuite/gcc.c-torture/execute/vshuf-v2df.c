#if __SIZEOF_DOUBLE__ == 8 && __SIZEOF_LONG_LONG__ == 8
typedef double V __attribute__((vector_size(16)));
typedef unsigned long long VI __attribute__((vector_size(16)));
#else
#define UNSUPPORTED
#endif

#define A	0.69314718055994530942
#define B	2.7182818284590452354

#define X	3.14159265358979323846
#define Y	1.41421356237309504880

#include "vshuf-2.inc"
#include "vshuf-main.inc"
