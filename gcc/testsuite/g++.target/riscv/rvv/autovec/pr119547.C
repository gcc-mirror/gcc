/* { dg-do run { target rv64 } } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d --param=logical-op-non-short-circuit=0" } */

#include <riscv_vector.h>
using v_int32 = vint32m2_t;
using v_float64 = vfloat64m2_t;
struct Size
{
  int width;
  int height;
};
template <class> struct VTraits
{
  static int vlanes () { return __riscv_vsetvlmax_e32m2 (); }
};
v_int32
v_load_expand_q (const signed char *ptr)
{
  return __riscv_vwcvt_x (
    __riscv_vwcvt_x (__riscv_vle8_v_i8mf2 (ptr, VTraits<v_int32>::vlanes ()),
		     VTraits<v_int32>::vlanes ()),
    VTraits<v_int32>::vlanes ());
}
v_float64
v_cvt_f64_high (v_int32 a)
{
  return __riscv_vget_f64m2 (__riscv_vfwcvt_f (a, VTraits<v_int32>::vlanes ()),
			     1);
}
void
v_store (double *ptr, v_float64 a)
{
  __riscv_vse64 (ptr, a, __riscv_vsetvlmax_e64m2 ());
}
void
v_store_pair_as (double *ptr, v_float64 b)
{
  v_store (ptr, b);
}
void
vx_load_pair_as (const signed char *ptr, v_float64, v_float64 &b)
{
  v_int32 v0;
  b = v_cvt_f64_high (v0);
};
void
cvt_64f (const signed char *src, double *dst, Size size)
{
  int VECSZ = __riscv_vsetvlmax_e64m2 ();
  for (int i; i < size.height; i++)
    {
      int j;
      for (;; j += VECSZ)
	{
	  if (j > -VECSZ)
	    if (j == 0 || dst)
	      break;
	  v_float64 v0, v1;
	  vx_load_pair_as (src, v0, v1);
	  v_store_pair_as (dst, v1);
	}
      for (; j < size.width; j++)
	dst[j] = (src[j]);
    }
}
void
cvtScale8s64f (unsigned char *src_, unsigned char *dst_,
	       size_t, Size size, void *)
{
  signed char src;
  double dst = *dst_;
  cvt_64f (&src, &dst, size);
}
int main ()
{
  unsigned char src[1];
  unsigned char dst[1024];
  double scale[1];
  Size size{4, 1};
  cvtScale8s64f (src, dst, 32, size, scale);
}
