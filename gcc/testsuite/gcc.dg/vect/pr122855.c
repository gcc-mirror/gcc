/* { dg-do compile { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-O3 -march=haswell" } */

int zoom_x3_weights_0, zoom_x3_j, zoom_x3_pixel2;

void zoom_x3(char *__restrict s, char *__restrict zoom_x3_tmp) {
  int pixel0 = 0, pixel1 = 0;
  for (; zoom_x3_j; zoom_x3_j--) {
    pixel0 += *s++ * zoom_x3_weights_0;
    pixel1 += *s++ * zoom_x3_weights_0;
    zoom_x3_pixel2 += *s++ * zoom_x3_weights_0;
  }
  *zoom_x3_tmp++ = pixel0 < 0 ? 0 : pixel0 > 255 ? 255 : pixel0;
  *zoom_x3_tmp = pixel1 < 0 ? 0 : pixel1 > 255 ? 255 : pixel1;
}
