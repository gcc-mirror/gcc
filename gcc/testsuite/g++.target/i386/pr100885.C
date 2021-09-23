/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-mavx512vl -mno-avx512bw -O2 -Wno-int-to-pointer-cast -std=c++14" } */

#include<x86intrin.h>
typedef unsigned char byte;
enum ZoomLevel { ZOOM_LVL_COUNT };
struct Colour {
  unsigned data;
  Colour(int data) : data(data) {}
};
struct Palette {
  Colour palette[6];
};
enum BlitterMode { BM_COLOUR_REMAP };
class Blitter {
public:
  struct BlitterParams {
    int width;
    int height;
    int left;
    int top;
    void *dst;
  };
  virtual void Draw();
};
class Blitter_32bppAnim : public Blitter {
protected:
  unsigned short anim_buf;
  Palette palette;
  int LookupColourInPalette_index;
  Colour LookupColourInPalette() {
    return palette.palette[LookupColourInPalette_index];
  }
};
__m128i _mm_set_epi64(__m64 __q0) {
  __m128i __trans_tmp_5{(long long)__q0};
  return __trans_tmp_5;
}
long _mm_storel_epi64___P, Draw_dsts;
__m128i _mm_packus_epi16___B, _mm_subs_epu16___B, _mm_hadd_epi16___Y,
    Draw_srcABCD, Draw___trans_tmp_10, Draw___trans_tmp_29, Draw___trans_tmp_7,
    AlphaBlendTwoPixels___trans_tmp_12, AlphaBlendTwoPixels___trans_tmp_11,
    AdjustBrightnessOfTwoPixels_from;
int _mm_srli_epi16___B;
class Blitter_32bppSSE_Base {
public:
  enum ReadMode { RM_WITH_MARGIN };
  enum BlockType { BT_NONE };
  struct SpriteData {
    int infos[ZOOM_LVL_COUNT];
    byte data;
  };
};
byte *Draw_remap;
short Draw_si_0;
class Blitter_32bppSSE4_Anim : Blitter_32bppAnim, Blitter_32bppSSE_Base {
  template <BlitterMode, ReadMode, BlockType, bool, bool>
  void Draw(const BlitterParams *, ZoomLevel);
  void Draw();
};
__m128i AdjustBrightnessOfTwoPixels() {
  __m128i __trans_tmp_28, __trans_tmp_27, __trans_tmp_26, __trans_tmp_24,
      __trans_tmp_23, __trans_tmp_22, __trans_tmp_21, __trans_tmp_20,
      __trans_tmp_19, __trans_tmp_18, __trans_tmp_17, __trans_tmp_16,
      __trans_tmp_14 = _mm_srli_epi16(AdjustBrightnessOfTwoPixels_from,
				      _mm_srli_epi16___B),
      __trans_tmp_7;
  char __trans_tmp_8;
  __trans_tmp_7 = __m128i{__trans_tmp_8};
  {
    __m128i __trans_tmp_7;
    char __trans_tmp_8;
    __trans_tmp_7 = __m128i{__trans_tmp_8};
    __trans_tmp_26 = __trans_tmp_7;
  }
  __trans_tmp_16 = (__v8hi)__trans_tmp_14 > (__v8hi)__trans_tmp_26;
  __trans_tmp_17 = _mm_hadd_epi16(__trans_tmp_16, _mm_hadd_epi16___Y);
  __trans_tmp_18 = _mm_hadd_epi16(__trans_tmp_17, _mm_hadd_epi16___Y);
  __trans_tmp_19 = _mm_srli_epi16(__trans_tmp_18, _mm_srli_epi16___B);
  {
    __m128i __trans_tmp_7;
    char __trans_tmp_8;
    __trans_tmp_7 = __m128i{__trans_tmp_8};
    __trans_tmp_27 = __trans_tmp_7;
  }
  __trans_tmp_20 = _mm_shuffle_epi8(__trans_tmp_19,
				    __trans_tmp_27);
  {
    __m128i __trans_tmp_7;
    char __trans_tmp_8;
    __trans_tmp_7 = __m128i{__trans_tmp_8};
    __trans_tmp_28 = __trans_tmp_7;
  }
  __trans_tmp_21 = _mm_subs_epu16(__trans_tmp_28, _mm_subs_epu16___B);
  __trans_tmp_22 = __m128i((__v8hu)__trans_tmp_21 * (__v8hu)__trans_tmp_20);
  __trans_tmp_23 = __m128i((__v8hu)__trans_tmp_22 + (__v8hu)__trans_tmp_7);
  __trans_tmp_24 = _mm_packus_epi16(__trans_tmp_23, _mm_packus_epi16___B);
  return __trans_tmp_24;
}
template <BlitterMode, Blitter_32bppSSE_Base::ReadMode,
          Blitter_32bppSSE_Base::BlockType, bool, bool>
void Blitter_32bppSSE4_Anim::Draw(const BlitterParams *bp, ZoomLevel zoom) {
  __m128i __trans_tmp_30;
  Colour *dst_line = (Colour *)bp->dst + bp->left;
  unsigned short *anim_line = &anim_buf + bp->top;
  int effective_width;
  SpriteData *sd = (SpriteData *)bp;
  Colour *src_rgba_line = (Colour *)sd->data;
  Draw___trans_tmp_29 = Draw___trans_tmp_7;
  for (int y = bp->height; y; y--) {
    Colour *dst = dst_line;
    unsigned short *anim = anim_line;
    anim += src_rgba_line[0].data;
    dst += src_rgba_line[0].data;
    int width_diff = Draw_si_0 - bp->width;
    effective_width = width_diff ?: effective_width;
    for (int x = effective_width; x; x--) {
      int mvX2 = *(unsigned *)sd->infos[zoom], m = byte(mvX2);
      __trans_tmp_30 = _mm_set_epi64(*(__m64_u *)dst);
      Colour c0 = Draw_dsts, srcm(0), cmap = LookupColourInPalette().data & 40;
      c0 = Draw_remap[m] ?: cmap;
      c0 = m ? c0 : srcm;
      Draw___trans_tmp_10 = __v2di{c0.data};
      if (mvX2)
        Draw_srcABCD = AdjustBrightnessOfTwoPixels();
      if (src_rgba_line)
        anim[1] = 0;
      __m128i tmp;
      __m128i dstAB = _mm_unpacklo_epi8(__trans_tmp_30, tmp);
      AlphaBlendTwoPixels___trans_tmp_12 =
          __m128i((__v8hu)Draw_srcABCD + (__v8hu)dstAB);
      AlphaBlendTwoPixels___trans_tmp_11 = _mm_shuffle_epi8 (AlphaBlendTwoPixels___trans_tmp_12,
							     Draw___trans_tmp_7);
      *(__m64_u *)_mm_storel_epi64___P =
          (__m64)AlphaBlendTwoPixels___trans_tmp_11[0];
    }
  }
}
Blitter::BlitterParams Draw_bp;
ZoomLevel Draw_zoom;
void Blitter_32bppSSE4_Anim::Draw() {
  Draw<BM_COLOUR_REMAP, RM_WITH_MARGIN, BT_NONE, true, false>(&Draw_bp,
                                                              Draw_zoom);
}
