/* { dg-do compile { target { aarch64*-*-* } } } */
/* { dg-options "-march=armv8-a+sve -w -Ofast" } */

typedef struct {
  int red;
} MagickPixelPacket;

int GetImageChannelMoments_image, GetImageChannelMoments_image_0,
    GetImageChannelMoments___trans_tmp_1, GetImageChannelMoments_M11_0,
    GetImageChannelMoments_pixel_3, GetImageChannelMoments_y,
    GetImageChannelMoments_p;

double GetImageChannelMoments_M00_0, GetImageChannelMoments_M00_1,
    GetImageChannelMoments_M01_1;

MagickPixelPacket GetImageChannelMoments_pixel;

void
SetMagickPixelPacket(int color, MagickPixelPacket *pixel) {
  pixel->red = color;
}

void
GetImageChannelMoments() {
  for (; GetImageChannelMoments_y; GetImageChannelMoments_y++) {
    SetMagickPixelPacket(GetImageChannelMoments_p,
                         &GetImageChannelMoments_pixel);
    GetImageChannelMoments_M00_1 += GetImageChannelMoments_pixel.red;
    if (GetImageChannelMoments_image)
      GetImageChannelMoments_M00_1++;
    GetImageChannelMoments_M01_1 +=
        GetImageChannelMoments_y * GetImageChannelMoments_pixel_3;
    if (GetImageChannelMoments_image_0)
      GetImageChannelMoments_M00_0++;
    GetImageChannelMoments_M01_1 +=
        GetImageChannelMoments_y * GetImageChannelMoments_p++;
  }
  GetImageChannelMoments___trans_tmp_1 = __builtin_atan(GetImageChannelMoments_M11_0);
}
