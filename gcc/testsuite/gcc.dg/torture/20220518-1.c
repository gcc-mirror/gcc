/* { dg-do compile } */
/* { dg-additional-options "-funswitch-loops" } */

enum {
  MOD_WVG_MASK_TEX_USE_INT,
  MOD_WVG_MASK_TEX_USE_RED,
  MOD_WVG_MASK_TEX_USE_BLUE,
  MOD_WVG_MASK_TEX_USE_SAT,
  MOD_WVG_MASK_TEX_USE_VAL,
  MOD_WVG_MASK_TEX_USE_ALPHA
} foo_num;
float *foo_org_w;
int *foo_new_w;
float foo_fact;
int foo_tex_use_channel, foo_i, foo_texres_0;
void foo()
{
  for (; foo_num;)
    switch (foo_tex_use_channel) {
    case MOD_WVG_MASK_TEX_USE_INT:
      foo_org_w[foo_i] = foo_new_w[foo_i] * foo_texres_0;
      break;
    case MOD_WVG_MASK_TEX_USE_RED:
      foo_org_w[foo_i] = 0;
    case MOD_WVG_MASK_TEX_USE_BLUE:
      foo_org_w[foo_i] = foo_fact + foo_org_w[foo_i];
      break;
    case MOD_WVG_MASK_TEX_USE_SAT:
      foo_org_w[foo_i] = foo_fact;
      break;
    case MOD_WVG_MASK_TEX_USE_VAL:
      foo_org_w[foo_i] = 0;
    case MOD_WVG_MASK_TEX_USE_ALPHA:
      foo_org_w[foo_i] = foo_fact + foo_org_w[foo_i];
      break;
    default:
      foo_org_w[foo_i] = foo_new_w[foo_i] * foo_texres_0;
    }
}
