/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-options "-O0 -msse" } */

typedef void __vr __attribute__ ((__mode__ (__V4SF__)));

struct vector
{
  union
  {
    __vr v;
    float f[4];
  };
};

void
doit ()
{
  float f[4];
  struct vector v;

  f[0] = 0;
  f[1] = 1;
  f[2] = 2;
  f[3] = 3;

  v.v = __builtin_ia32_loadups (f);
}
