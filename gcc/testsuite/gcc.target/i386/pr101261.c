/* PR middle-end/101261 */
/* { dg-do compile { target fpic } } */
/* { dg-options "-fno-semantic-interposition -fPIC" } */
/* { dg-require-ifunc "" } */

void
__attribute__((target_clones("default", "avx2")))
dt_ioppr_transform_image_colorspace()
{
  dt_ioppr_transform_image_colorspace();
}
