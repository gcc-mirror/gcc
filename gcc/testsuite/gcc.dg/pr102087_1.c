/* PR tree-optimization/102087 */
/* { dg-do compile } */
/* { dg-options "-O3 -fprefetch-loop-arrays -w" { target x86_64-*-* powerpc*-*-* } } */

char **Gif_ClipImage_gfi_0;
int Gif_ClipImage_gfi_1, Gif_ClipImage_y, Gif_ClipImage_shift;
void Gif_ClipImage() {
  Gif_ClipImage_y = Gif_ClipImage_gfi_1 - 1;
  for (; Gif_ClipImage_y >= Gif_ClipImage_shift; Gif_ClipImage_y++)
    Gif_ClipImage_gfi_0[Gif_ClipImage_shift] =
        Gif_ClipImage_gfi_0[Gif_ClipImage_y];
}

