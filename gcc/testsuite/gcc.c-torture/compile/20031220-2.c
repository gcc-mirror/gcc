/*  PR target/12749
  Orgin: Matt Thomas <matt@3am-software.com>
  This used to cause GCC to write out an instruction for i386 when using a L64 host
  which gas could not handle because GCC would write a full 64bit hex string out. */


float fabsf (float);
typedef int __int32_t;
typedef unsigned int __uint32_t;
typedef union
{
  float value;
  __uint32_t word;
} ieee_float_shape_type;
extern float __ieee754_expf (float);
extern float __ieee754_sinhf (float);
static const float one = 1.0, shuge = 1.0e37;
float
__ieee754_sinhf(float x)
{
        float t,w,h;
        __int32_t ix,jx;
        do { ieee_float_shape_type gf_u; gf_u.value = (x); (jx) = gf_u.word; } while (0);
        ix = jx&0x7fffffff;
        if(ix>=0x7f800000) return x+x;
        h = 0.5;
        if (jx<0) h = -h;
        if (ix < 0x41b00000) {
            if (ix<0x31800000)
                if(shuge+x>one) return x;
            t = expm1f(fabsf(x));
            if(ix<0x3f800000) return h*((float)2.0*t-t*t/(t+one));
            return h*(t+t/(t+one));
        }
        if (ix < 0x42b17180) return h*__ieee754_expf(fabsf(x));
        if (ix<=0x42b2d4fc) {
            w = __ieee754_expf((float)0.5*fabsf(x));
            t = h*w;
            return t*w;
        }
        return x*shuge;
}


