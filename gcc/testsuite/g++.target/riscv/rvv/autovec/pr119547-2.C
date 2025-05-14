/* { dg-do run { target rv64 } } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-options "-O3 -march=rv64gcv -mabi=lp64d --param=logical-op-non-short-circuit=0" } */

#include <riscv_vector.h>

using v_uint8 = vuint8m2_t;
using v_int8 = vint8m2_t;
using v_uint16 = vuint16m2_t;
using v_int16 = vint16m2_t;
using v_uint32 = vuint32m2_t;
using v_int32 = vint32m2_t;
using v_uint64 = vuint64m2_t;
using v_int64 = vint64m2_t;
using v_float32 = vfloat32m2_t;
using v_float64 = vfloat64m2_t;

using uchar = unsigned char;
using schar = signed char;
using ushort = unsigned short;
using uint = unsigned int;
using uint64 = unsigned long int;
using int64 = long int;

struct Size
{
  int width;
  int height;
};

template <class T> struct VTraits;

template <> struct VTraits<vint32m1_t>
{
  static inline int vlanes () { return __riscv_vsetvlmax_e32m1 (); }
  using lane_type = int32_t;
  static const int max_nlanes = 1024 / 32 * 2;
};
template <> struct VTraits<vint32m2_t>
{
  static inline int vlanes () { return __riscv_vsetvlmax_e32m2 (); }
  using lane_type = int32_t;
  static const int max_nlanes = 1024 / 32 * 2;
};
template <> struct VTraits<vint32m4_t>
{
  static inline int vlanes () { return __riscv_vsetvlmax_e32m4 (); }
  using lane_type = int32_t;
  static const int max_nlanes = 1024 / 32 * 2;
};
template <> struct VTraits<vint32m8_t>
{
  static inline int vlanes () { return __riscv_vsetvlmax_e32m8 (); }
  using lane_type = int32_t;
  static const int max_nlanes = 1024 / 32 * 2;
};

template <> struct VTraits<vfloat64m1_t>
{
  static inline int vlanes () { return __riscv_vsetvlmax_e64m1 (); }
  using lane_type = double;
  static const int max_nlanes = 1024 / 64 * 2;
};
template <> struct VTraits<vfloat64m2_t>
{
  static inline int vlanes () { return __riscv_vsetvlmax_e64m2 (); }
  using lane_type = double;
  static const int max_nlanes = 1024 / 64 * 2;
};
template <> struct VTraits<vfloat64m4_t>
{
  static inline int vlanes () { return __riscv_vsetvlmax_e64m4 (); }
  using lane_type = double;
  static const int max_nlanes = 1024 / 64 * 2;
};
template <> struct VTraits<vfloat64m8_t>
{
  static inline int vlanes () { return __riscv_vsetvlmax_e64m8 (); }
  using lane_type = double;
  static const int max_nlanes = 1024 / 64 * 2;
};

static inline v_float64
v_setall_f64 (double v)
{
  return __riscv_vfmv_v_f_f64m2 (v, VTraits<v_float64>::vlanes ());
}
static inline v_float64
vx_setall_f64 (double v)
{
  return v_setall_f64 (v);
}

inline v_int32
v_load_expand_q (const schar *ptr)
{
  return __riscv_vwcvt_x (
    __riscv_vwcvt_x (__riscv_vle8_v_i8mf2 (ptr, VTraits<v_int32>::vlanes ()),
		     VTraits<v_int32>::vlanes ()),
    VTraits<v_int32>::vlanes ());
}

static inline v_int32
vx_load_expand_q (const schar *ptr)
{
  return v_load_expand_q (ptr);
}

inline v_float64
v_cvt_f64 (const v_int32 &a)
{
  return __riscv_vget_f64m2 (__riscv_vfwcvt_f (a, VTraits<v_int32>::vlanes ()),
			     0);
}

inline v_float64
v_cvt_f64_high (const v_int32 &a)
{
  return __riscv_vget_f64m2 (__riscv_vfwcvt_f (a, VTraits<v_int32>::vlanes ()),
			     1);
}

inline void
v_store (double *ptr, const v_float64 &a)
{
  __riscv_vse64 (ptr, a, VTraits<v_float64>::vlanes ());
}

static inline void
v_store_pair_as (double *ptr, const v_float64 &a, const v_float64 &b)
{
  v_store (ptr, a);
  v_store (ptr + VTraits<v_float64>::vlanes (), b);
}

static inline void
vx_load_pair_as (const schar *ptr, v_float64 &a, v_float64 &b)
{
  v_int32 v0 = vx_load_expand_q (ptr);
  a = v_cvt_f64 (v0);
  b = v_cvt_f64_high (v0);
}

inline v_float64
v_fma (const v_float64 &a, const v_float64 &b, const v_float64 &c)
{
  return __riscv_vfmacc_vv_f64m2 (c, a, b, VTraits<v_float64>::vlanes ());
}

template <typename _Tp>
static inline _Tp
saturate_cast (double v)
{
  return _Tp (v);
}

template <typename _Ts, typename _Td>
__attribute__ ((noipa)) void
cvt_64f (const _Ts *src, size_t sstep, _Td *dst, size_t dstep, Size size,
	 double a, double b)
{
  v_float64 va = vx_setall_f64 (a), vb = vx_setall_f64 (b);
  const int VECSZ = VTraits<v_float64>::vlanes () * 2;

  sstep /= sizeof (src[0]);
  dstep /= sizeof (dst[0]);

  for (int i = 0; i < size.height; i++, src += sstep, dst += dstep)
    {
      int j = 0;

      for (; j < size.width; j += VECSZ)
	{
	  if (j > size.width - VECSZ)
	    {
	      if (j == 0 || src == (_Ts *) dst)
		break;
	      j = size.width - VECSZ;
	    }
	  v_float64 v0, v1;
	  vx_load_pair_as (src + j, v0, v1);
	  v0 = v_fma (v0, va, vb);
	  v1 = v_fma (v1, va, vb);
	  v_store_pair_as (dst + j, v0, v1);
	}

      for (; j < size.width; j++)
	dst[j] = saturate_cast<_Td> (src[j] * a + b);
    }
}

void
__attribute__ ((noipa))
cvtScale8s64f (const uchar *src_, size_t sstep, const uchar *, size_t,
	       uchar *dst_, size_t dstep, Size size, void *scale_)
{
  const schar *src = (const schar *) src_;
  double *dst = (double *) dst_;
  double *scale = (double *) scale_;
  cvt_64f (src, sstep, dst, dstep, size, (double) scale[0], (double) scale[1]);
}

int main ()
{
  uchar src[1024];
  uchar dst[1024];

  double scale[2] = {2.0, 3.0};
  Size size {4, 1};

  cvtScale8s64f (src, 4, NULL, 0, dst, 32, size, (void *)scale);
}
