/* PR middle-end/115887 */
/* { dg-do compile { target { bitint && int128 } } } */
/* { dg-options "-O -fnon-call-exceptions -finstrument-functions -w" } */

float f;
#if __BITINT_MAXWIDTH__ >= 1024
#define N1024 1024
#define N127 127
#define N256 256
#else
#define N1024 64
#define N127 64
#define N256 64
#endif

_BitInt(N1024) a;

static inline void
bar (_BitInt(N127) b, _BitInt(N256) c, int,
     int, int, int, int, int, int, int, int, 
     int, int, int, int, int, int, int, int,
     int *)
{
  b %= 0;
  do
    c -= *(short *) 0;
  while (__builtin_add_overflow_p (a, 0, 0));
  __int128 d = b + c + f;
}

void
foo (void)
{
  int x;
  bar (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, &x);
  while (x)
    ;
}
