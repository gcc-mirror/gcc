/*
 * inspired by glibc-2.0.6/sysdeps/libm-ieee754/s_nextafterf.c
 *
 * gcc -O2 -S -DOP=+ gives faddp %st(1),%st
 * gcc -O2 -S -DOP=* gives fmulp %st(1),%st
 * gcc -O2 -S -DOP=- gives fsubrp %st(1),%st
 * gcc -O2 -S -DOP=/ gives fdivrp %st(1),%st
 */

#ifndef OP
#define OP *
#endif

typedef int int32_t __attribute__ ((__mode__ (  __SI__ ))) ;
typedef unsigned int u_int32_t __attribute__ ((__mode__ (  __SI__ ))) ;

typedef union
{
  float value;
  u_int32_t word;
} ieee_float_shape_type;

float __nextafterf(float x, float y)
{
 int32_t hx,hy,ix,iy;

 {
  ieee_float_shape_type gf_u;
  gf_u.value = x;
  hx = gf_u.word;
 }
 {
  ieee_float_shape_type gf_u;
  gf_u.value = y;
  hy = gf_u.word;
 }
 ix = hx&0x7fffffff;
 iy = hy&0x7fffffff;

 if ( ix > 0x7f800000 || iy > 0x7f800000 )
    return x+y;
 if (x == y) return x;
 if (ix == 0)
   {
    {
     ieee_float_shape_type sf_u;
     sf_u.word = (hy&0x80000000) | 1;
     x = sf_u.value;
    }
    y = x*x;
    if (y == x) return y; else return x;
   }
 if (hx >= 0)
   {
    if (hx > hy)
       hx -= 1;
    else
       hx += 1;
   }
 else
   {
    if (hy >= 0 || hx > hy)
       hx -= 1;
    else
       hx += 1;
   }
 hy = hx & 0x7f800000;
 if (hy >= 0x7f800000)
    return x+x;
 if (hy < 0x00800000)
   {
    y = x OP x;
    if (y != x)
      {
       ieee_float_shape_type sf_u;
       sf_u.word = hx;
       y = sf_u.value;
       return y;
      }
   }
 {
  ieee_float_shape_type sf_u;
  sf_u.word = hx;
  x = sf_u.value;
 }
 return x;
}


