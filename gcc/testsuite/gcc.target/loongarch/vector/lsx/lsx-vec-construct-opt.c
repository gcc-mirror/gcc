/* { dg-do compile } */
/* { dg-options "-mlsx -O3" } */

#include <lsxintrin.h>

extern long long *x_di;
extern int *x_si;
extern short int *x_hi;
extern char *x_qi;
extern double *y_df;
extern float *y_sf;

/* No change for V2DI mode.  */
v2i64
vec_construct_v2i64 ()
{
  v2i64 res =
  { x_di[1], x_di[0]}
  ;
  return res;
}

/* Only load the lowest 2 elements and directly copy them to high half-part,
   reducing more vinsgr2vr.w.  */
/* { dg-final { scan-assembler-times "v4i32:.*\tvreplvei\\.d.*v4i32" 1 } } */
v4i32
vec_construct_v4i32 ()
{
  v4i32 res =
  { x_si[0], x_si[1], x_si[0], x_si[1]} 
  ;
  return res;
}

/* Only load the lowest 4 elements and directly copy them to high half-part,
   reducing more vinsgr2vr.h.  */
/* { dg-final { scan-assembler-times "v8i16:.*\tvreplvei\\.d.*v8i16" 1 } } */
v8i16
vec_construct_v8i16 ()
{
  v8i16 res =
  { x_hi[0], x_hi[0], x_hi[0], x_hi[1], 
    x_hi[0], x_hi[0], x_hi[0], x_hi[1] }
  ;
  return res;
}

/* Only load the lowest 8 elements and directly copy them to high half-part,
   reducing more vinsgr2vr.b.  */
/* { dg-final { scan-assembler-times "v16i8:.*\tvreplvei\\.d.*v16i8" 1 } } */
v16i8
vec_construct_v16i8 ()
{
  v16i8 res =
  { x_qi[0], x_qi[1], x_qi[0], x_qi[2], 
    x_qi[0], x_qi[0], x_qi[0], x_qi[3],
    x_qi[0], x_qi[1], x_qi[0], x_qi[2], 
    x_qi[0], x_qi[0], x_qi[0], x_qi[3] }
  ;
  return res;
}

/* Set 2 elements of a vector simultaneously by vilvl.d.  */
/* { dg-final { scan-assembler-not "v2f64:.*\tvextrins\\.d.*v2f64" } } */
/* { dg-final { scan-assembler-times "v2f64:.*\tvilvl\\.d.*v2f64" 1 } } */
v2f64
vec_construct_v2f64 ()
{
  v2f64 res =
  { y_df[0], y_df[2] } 
  ;
  return res;
}

/* Set 2 elements of a vector simultaneously by vilvl.w
   and reducing more vextrins.w.  */
/* { dg-final { scan-assembler-times "v4f32:.*\tvilvl\\.w.*v4f32" 1 } } */
v4f32
vec_construct_v4f32 ()
{
  v4f32 res =
  { y_sf[0], y_sf[1], y_sf[0], y_sf[0] }
  ;
  return res;
}
