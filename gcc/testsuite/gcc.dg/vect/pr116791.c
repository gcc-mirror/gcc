/* { dg-do compile } */
/* { dg-additional-options "-mavx2" { target avx2 } } */

struct nine_context {
  unsigned tex_stage[8][33];
};
struct fvec4 {
  float x[2];
};
void f(struct fvec4 *dst, struct nine_context *context)
{
  unsigned s;
  for (s = 0; s < 8; ++s)
    {
      float *rgba = &dst[s].x[0];
      unsigned color = context->tex_stage[s][0];
      rgba[0] = (float)((color >> 16) & 0xFF) / 0xFF;
      rgba[1] = (float)((color >> 8) & 0xFF) / 0xFF;
    }
}
