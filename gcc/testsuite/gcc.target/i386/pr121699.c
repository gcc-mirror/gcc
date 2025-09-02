/* { dg-do compile } */
/* { dg-options "-march=znver4 -O3" } */

typedef struct
{
  int u32;
} nir_const_value;

nir_const_value *evaluate_prmt_nv__dst_val;

int evaluate_prmt_nv__src_0, evaluate_prmt_nv_src;

void
evaluate_prmt_nv (unsigned num_components)
{
  for (unsigned _i = 0; _i < num_components; _i++)
    {
      char x = evaluate_prmt_nv_src;
      if (evaluate_prmt_nv__src_0)
        x = x >> 7;
      evaluate_prmt_nv__dst_val[_i].u32 = x;
    }
}
