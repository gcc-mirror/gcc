/* { dg-do compile } */
/* { dg-options "-mlasx -O3" } */

#include <lasxintrin.h>

extern long long *x_di;
extern int *x_si;
extern short int *x_hi;
extern char *x_qi;
extern double *y_df;
extern float *y_sf;

/* Remove some unnecessary vinsgr2vr.d as the corresponding elements
   have already been set.  */
/* { dg-final { scan-assembler-not "v4i64:.*\tvinsgr2vr\\.d.*v4i64" } } */
/* { dg-final { scan-assembler-times "v4i64:.*\txvldrepl\\.d.*v4i64" 1 } } */
v4i64
vec_construct_v4i64 ()
{
  v4i64 res =
  { x_di[0], x_di[0], x_di[1], x_di[1] }
  ;
  return res;
}

/* Remove some unnecessary vinsgr2vr.w as the corresponding elements
   have already been set.  */
/* { dg-final { scan-assembler-not "v8i32:.*\tvinsgr2vr\\.w.*v8i32" } } */
/* { dg-final { scan-assembler-times "v8i32:.*\txvreplgr2vr\\.w.*v8i32" 1 } } */
v8i32
vec_construct_v8i32 ()
{
  v8i32 res =
  { x_si[0], x_si[0], x_si[0], x_si[0], 
    x_si[0], x_si[2], x_si[0], x_si[0] }
  ;
  return res;
}

/* Remove some unnecessary vinsgr2vr.h as the corresponding elements
   have already been set.  */
/* { dg-final { scan-assembler-not "v16i16:.*\tvori\\.b.*v16i16" } } */
/* { dg-final { scan-assembler-times "v16i16:.*\txvreplgr2vr\\.h.*v16i1" 1 } } */
v16i16
vec_construct_v16i16 ()
{
  v16i16 res =
  { x_hi[1], x_hi[2], x_hi[1], x_hi[1], 
    x_hi[1], x_hi[1], x_hi[1], x_hi[1],
    x_hi[1], x_hi[1], x_hi[1], x_hi[1], 
    x_hi[1], x_hi[1], x_hi[1], x_hi[2] }
  ;
  return res;
}

/* Remove some unnecessary vinsgr2vr.b as the corresponding elements
   have already been set.  */
/* { dg-final { scan-assembler-not "v32i8:.*\tvori\\.b.*v32i8" } } */
/* { dg-final { scan-assembler-times "v32i8:.*\txvreplgr2vr\\.b.*v32i8" 1 } } */
v32i8
vec_construct_v32i8 ()
{
  v32i8 res =
  { x_qi[0], x_qi[0], x_qi[0], x_qi[0], 
    x_qi[0], x_qi[0], x_qi[0], x_qi[0],
    x_qi[0], x_qi[0], x_qi[0], x_qi[0],
    x_qi[0], x_qi[0], x_qi[0], x_qi[2],
    x_qi[0], x_qi[0], x_qi[0], x_qi[0], 
    x_qi[0], x_qi[0], x_qi[0], x_qi[0], 
    x_qi[0], x_qi[0], x_qi[0], x_qi[0], 
    x_qi[0], x_qi[0], x_qi[0], x_qi[3] }
  ;
  return res;
}

/* Set 2 elements of a vector simultaneously by vilvl.d
   and reducing more vextrins.d.  */
/* { dg-final { scan-assembler-not "v4f64:.*\tvori\\.b.*v4f64" } } */
/* { dg-final { scan-assembler-not "v4f64:.*\tvextrins\\.d.*v4f64" } } */
/* { dg-final { scan-assembler-times "v4f64:.*\tvilvl\\.d.*v4f64" 1 } } */
v4f64
vec_construct_v4f64 ()
{
  v4f64 res =
  { y_df[0], y_df[2], y_df[0], y_df[0]} 
  ;
  return res;
}

/* Set 2 elements of a vector simultaneously by vilvl.w
   and reducing more vextrins.w.  */
/* { dg-final { scan-assembler-not "v8f32:.*\tvextrins\\.w.*v8f32" } } */
/* { dg-final { scan-assembler-times "v8f32:.*\txvilvl\\.w.*v8f32" 1 } } */
v8f32
vec_construct_v8f32 ()
{
  v8f32 res =
  { y_sf[2], y_sf[1], y_sf[2], y_sf[3], 
    y_sf[2], y_sf[1], y_sf[2], y_sf[3] }
  ;
  return res;
}
