// { dg-options "-std=c++11" }
// // { dg-do run }
#if __SIZEOF_FLOAT__ == 4
typedef float V __attribute__((vector_size(8)));
# if __SIZEOF_INT__ == 4
typedef unsigned int VI __attribute__((vector_size(8)));
# elif __SIZEOF_LONG__ == 4
typedef unsigned long VI __attribute__((vector_size(8)));
# else
#  define UNSUPPORTED
# endif
#else
# define UNSUPPORTED
#endif

#define A	0.69314718055994530942f
#define B	2.7182818284590452354f

#define X	3.14159265358979323846f
#define Y	1.41421356237309504880f

#include "vshuf-2.inc"
#include "vshuf-main.inc"
