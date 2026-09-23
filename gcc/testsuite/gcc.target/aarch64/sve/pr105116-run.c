/* PR target/105116 */
/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -fno-split-wide-types" } */

/* Besides the ICE, the non-representable high-lane subregs used to be
   resolved lossily (silently reading lane 0), miscompiling the upper lanes.
   Check the scalarised divide produces the correct result for every lane.  */

typedef signed char vnx16qi __attribute__((vector_size (32)));

__attribute__((noipa)) void
vdiv_vnx16qi (vnx16qi *x, vnx16qi y, vnx16qi z)
{
  register vnx16qi dst  asm ("z0");
  register vnx16qi src1 asm ("z2");
  register vnx16qi src2 asm ("z4");
  dst = *x;
  src1 = y;
  src2 = z;
  asm volatile ("" :: "w" (dst), "w" (src1), "w" (src2));
  dst = src2 - (dst / src1);
  asm volatile ("" :: "w" (dst));
  *x = dst;
}

int
main (void)
{
  vnx16qi X, Y, Z;
  signed char xv[32], yv[32], zv[32];
  for (int i = 0; i < 32; i++)
    {
      xv[i] = (signed char) (i * 7 - 50);
      yv[i] = (signed char) (((i % 5) + 1) * (i & 1 ? 1 : -1));
      zv[i] = (signed char) (100 - i * 3);
    }
  __builtin_memcpy (&X, xv, 32);
  __builtin_memcpy (&Y, yv, 32);
  __builtin_memcpy (&Z, zv, 32);
  vdiv_vnx16qi (&X, Y, Z);
  signed char got[32];
  __builtin_memcpy (got, &X, 32);
  for (int i = 0; i < 32; i++)
    if (got[i]
	!= (signed char) (zv[i] - (signed char) ((int) xv[i] / (int) yv[i])))
      __builtin_abort ();
  return 0;
}
