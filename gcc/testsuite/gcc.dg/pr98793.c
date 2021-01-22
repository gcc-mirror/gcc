/* { dg-do run } */
/* { dg-options "-O2 -Wno-psabi" } */

typedef unsigned char u8;
typedef unsigned char __attribute__((__vector_size__ (8))) v64u8;
typedef unsigned char __attribute__((__vector_size__ (64))) v512u8;
typedef unsigned long long u64;

u64 gx;
v512u8 gu;
v512u8 gv;

v64u8 __attribute__((__noipa__)) foo0 (u8 ax, v512u8 au, u64 ay)
{
  u8 lx = ax && (u8) ay;
  v512u8 lc = 7 <= au;
  v512u8 ld = (u8) (ay && gx) == gu;
  v512u8 le = (v512u8) ld + (v512u8) gv;
  v512u8 lf = le + lc;
  return (((union
            {
            v512u8 a;
            v64u8 b[8];
            }) lf).b[3]) + lx;
}

int
main (void)
{
  v64u8 x = foo0 (2, (v512u8) { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 15
                  },
                  2);
  for (unsigned i = 0; i < sizeof (x); i++)
    if (x[i] != (i ? 0 : 0xff))
      __builtin_abort ();
  return 0;
}
