/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v3" } */

extern union {
  int i;
  float f;
} int_as_float_u;

extern int render_result_from_bake_w;
extern int render_result_from_bake_h_seed_pass;
extern float *render_result_from_bake_h_primitive;
extern float *render_result_from_bake_h_seed;

float
int_as_float(int i)
{
  int_as_float_u.i = i;
  return int_as_float_u.f;
}

void
render_result_from_bake_h(int tx)
{
  while (render_result_from_bake_w) {
    for (; tx < render_result_from_bake_w; tx++)
      render_result_from_bake_h_primitive[1] =
          render_result_from_bake_h_primitive[2] = int_as_float(-1);
    if (render_result_from_bake_h_seed_pass) {
      *render_result_from_bake_h_seed = 0;
    }
  }
}
