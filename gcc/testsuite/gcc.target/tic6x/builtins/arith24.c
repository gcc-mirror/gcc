/* { dg-require-effective-target ti_c64xp } */

#include <c6x_intrinsics.h>

extern void abort (void);

typedef short  __v2hi __attribute ((vector_size(4)));

int a = 0x5000d000;
int b = 0xc0002000;
int c = 0x40009000;
int d = 0x80000001;
int e = 0x50002001;
int f = 0xc0008000;

int a4 = 0x50d03080;
int b4 = 0xc020f080;
int c4 = 0xc0202080;
int d4 = 0x50003080;
int e4 = 0xc0202180;

int main ()
{
  int v;
  long long vll;

  v = _add2 (a, b);
  if (v != 0x1000f000)
    abort ();
  v = _sub2 (a, b);
  if (v != 0x9000b000)
    abort ();
  v = _sub2 (b, a);
  if (v != 0x70005000)
    abort ();

  v = _add4 (a4, b4);
  if (v != 0x10f02000)
    abort ();
  v = _sub4 (a4, b4);
  if (v != 0x90b04000)
    abort ();
  v = _saddu4 (a4, c4);
  if (v != 0xfff050ff)
    abort ();

  v = _sadd2 (a, b);
  if (v != 0x1000f000)
    abort ();
  v = _sadd2 (a, c);
  if (v != 0x7fff8000)
    abort ();

  v = _ssub2 (a, b);
  if (v != 0x7fffb000)
    abort ();
  v = _ssub2 (b, a);
  if (v != 0x80005000)
    abort ();

  vll = _smpy2ll (a, b);
  if (vll != 0xd8000000f4000000ll)
    abort ();
  vll = _smpy2ll (d, d);
  if (vll != 0x7fffffff00000002ll)
    abort ();

  v = _avg2 (b, e);
  if (v != 0x08002001)
    abort ();
  v = _avgu4 (d4, e4);
  if (v != 0x88102980)
    abort ();

  v = _abs2 (a);
  if (v != 0x50003000)
    abort ();
  v = _abs2 (f);
  if (v != 0x40007fff)
    abort ();

  return 0;
}
