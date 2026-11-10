/* { dg-do compile } */
/* { dg-options "-O1" } */

typedef float v64sf __attribute__ ((vector_size (256)));
typedef double v64df __attribute__ ((vector_size (512)));
typedef int v64si __attribute__ ((vector_size (256)));
typedef long v64di __attribute__ ((vector_size (512)));

v64sf f (v64sf _x, v64si _y)
{
  v64sf x = _x;
  v64si y = _y;
  x = __builtin_gcn_fabsvf (x); /* { dg-final { scan-assembler "v_add_f32\\s+v\[0-9\]+, 0, |v\[0-9\]+|" } } */
  x = __builtin_gcn_floorvf (x); /* { dg-final { scan-assembler "v_floor_f32\\s+v\[0-9\]+, v\[0-9\]+" } }*/
  x = __builtin_gcn_frexpvf_mant (x); /* { dg-final { scan-assembler "v_frexp_mant_f32\\s+v\[0-9\]+, v\[0-9\]+" } }*/
  y = __builtin_gcn_frexpvf_exp (x); /* { dg-final { scan-assembler "v_frexp_exp_i32_f32\\s+v\[0-9\]+, v\[0-9\]+" } }*/
  x = __builtin_gcn_ldexpvf (x, y); /* { dg-final { scan-assembler "v_ldexp_f32\\s+v\[0-9\]+, v\[0-9\]+, v\[0-9\]+" } }*/

  return x;
}

v64df g (v64df _x, v64si _y)
{
  v64df x = _x;
  v64si y = _y;
  x = __builtin_gcn_fabsv (x); /* { dg-final { scan-assembler "v_add_f64\\s+v\\\[\[0-9\]+:\[0-9]+\\\], 0, |v\\\[\[0-9\]+:\[0-9\]+\\\]|" } } */
  x = __builtin_gcn_floorv (x); /* { dg-final { scan-assembler "v_floor_f64\\s+v\\\[\[0-9\]+:\[0-9]+\\\], v\\\[\[0-9\]+:\[0-9]+\\\]" } }*/
  x = __builtin_gcn_frexpv_mant (x); /* { dg-final { scan-assembler "v_frexp_mant_f64\\s+v\\\[\[0-9\]+:\[0-9]+\\\], v\\\[\[0-9\]+:\[0-9]+\\\]" } }*/
  y = __builtin_gcn_frexpv_exp (x); /* { dg-final { scan-assembler "v_frexp_exp_i32_f64\\s+v\[0-9\]+, v\\\[\[0-9\]+:\[0-9]+\\\]" } }*/
  x = __builtin_gcn_ldexpv (x, y); /* { dg-final { scan-assembler "v_ldexp_f64\\s+v\\\[\[0-9\]+:\[0-9]+\\\], v\\\[\[0-9\]+:\[0-9]+\\\], v\[0-9\]+" } }*/

  return x;
}
