#include <stdarg.h>

extern void abort (void);
extern void exit (int);

void bar (int n, int c)
{
  static int lastn = -1, lastc = -1;

  if (lastn != n)
    {
      if (lastc != lastn)
	abort ();
      lastc = 0;
      lastn = n;
    }

  if (c != (char) (lastc ^ (n << 3)))
    abort ();
  lastc++;
}

#define D(N) typedef struct { char x[N]; } A##N;
D(0) D(1) D(2) D(3) D(4) D(5) D(6) D(7)
D(8) D(9) D(10) D(11) D(12) D(13) D(14) D(15)
D(16) D(31) D(32) D(35) D(72)
#undef D

void foo (int size, ...)
{
#define D(N) A##N a##N;
D(0) D(1) D(2) D(3) D(4) D(5) D(6) D(7)
D(8) D(9) D(10) D(11) D(12) D(13) D(14) D(15)
D(16) D(31) D(32) D(35) D(72)
#undef D
  va_list ap;
  int i;

  if (size != 21)
    abort ();
  va_start (ap, size);
#define D(N)					\
  a##N = va_arg (ap, typeof (a##N));		\
  for (i = 0; i < N; i++)			\
    bar (N, a##N.x[i]);
D(0) D(1) D(2) D(3) D(4) D(5) D(6) D(7)
D(8) D(9) D(10) D(11) D(12) D(13) D(14) D(15)
D(16) D(31) D(32) D(35) D(72)
#undef D
  va_end (ap);
}

int main (void)
{
#define D(N) A##N a##N;
D(0) D(1) D(2) D(3) D(4) D(5) D(6) D(7)
D(8) D(9) D(10) D(11) D(12) D(13) D(14) D(15)
D(16) D(31) D(32) D(35) D(72)
#undef D
  int i;

#define D(N)					\
  for (i = 0; i < N; i++)			\
    a##N.x[i] = i ^ (N << 3);
D(0) D(1) D(2) D(3) D(4) D(5) D(6) D(7)
D(8) D(9) D(10) D(11) D(12) D(13) D(14) D(15)
D(16) D(31) D(32) D(35) D(72)
#undef D

  foo (21
#define D(N) , a##N
D(0) D(1) D(2) D(3) D(4) D(5) D(6) D(7)
D(8) D(9) D(10) D(11) D(12) D(13) D(14) D(15)
D(16) D(31) D(32) D(35) D(72)
#undef D
      );
  exit (0);
}
