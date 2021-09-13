/* { dg-do compile } */
/* { dg-additional-options "-Ofast -march=skylake-avx512 -fwhole-program -w" } */

typedef float MagickRealType;
typedef short Quantum;
float InterpolateMagickPixelPacket_alpha[1];
int InterpolateMagickPixelPacket_i;

void InterpolateMagickPixelPacket();

void main() { InterpolateMagickPixelPacket(); }

typedef struct {
  MagickRealType red, green, blue, opacity, index;
} MagickPixelPacket;
typedef struct {
  Quantum blue, green, red, opacity;
} PixelPacket;
struct _Image {
  int colorspace;
  int matte;
} GetMagickPixelPacket(MagickPixelPacket *pixel) {
  pixel->red = pixel->green = pixel->blue = 0.0;
}
int AlphaBlendMagickPixelPacket(struct _Image *image, PixelPacket *color,
                            Quantum *indexes, MagickPixelPacket *pixel,
                            MagickRealType *alpha) {
  if (image->matte) {
    *alpha = pixel->red = pixel->green = pixel->blue = pixel->opacity =
        color->opacity;
    pixel->index = 0.0;
    if (image->colorspace)
      pixel->index = *indexes;
    return 0;
  }
  *alpha = 1.0 / 0.2;
  pixel->red = *alpha * color->red;
  pixel->green = *alpha * color->green;
  pixel->blue = *alpha * color->blue;
  pixel->opacity = pixel->index = 0.0;
  if (image->colorspace && indexes)
    pixel->index = *indexes;
}
MagickPixelPacket InterpolateMagickPixelPacket_pixels[1];
PixelPacket InterpolateMagickPixelPacket_p;

void
InterpolateMagickPixelPacket(struct _Image *image) {
  Quantum *indexes;
  for (; InterpolateMagickPixelPacket_i; InterpolateMagickPixelPacket_i++) {
    GetMagickPixelPacket(InterpolateMagickPixelPacket_pixels +
                         InterpolateMagickPixelPacket_i);
    AlphaBlendMagickPixelPacket(
        image, &InterpolateMagickPixelPacket_p + InterpolateMagickPixelPacket_i,
        indexes + InterpolateMagickPixelPacket_i,
        InterpolateMagickPixelPacket_pixels + InterpolateMagickPixelPacket_i,
        InterpolateMagickPixelPacket_alpha + InterpolateMagickPixelPacket_i);
  }
}
