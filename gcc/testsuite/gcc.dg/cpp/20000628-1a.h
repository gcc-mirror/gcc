/* Included from 20000628-1.h.  This header is supposed to be seen
   exactly three times.  */

#if !defined A
#define A
int a;
#elif !defined B
#define B
int b;
#elif !defined C
#define C
int c;
#else
#error Included a fourth time
#endif
