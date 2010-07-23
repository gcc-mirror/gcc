/* { dg-do compile } */
/* { dg-options "-O0 -msse" } */

typedef float __vr __attribute__ ((vector_size (16)));

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
