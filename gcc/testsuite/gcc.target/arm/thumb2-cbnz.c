/* { dg-do assemble } */
/* { dg-options "-O1 -mthumb -march=armv7-a" } */

typedef short int int16_t;
typedef unsigned char uint8_t;
struct component
{
  float *Q_table;
};
static inline unsigned char descale_and_clamp(int x, int shift)
{
  x += (1UL<<(shift-1));
  if (x<0)
    x = (x >> shift) | ((~(0UL)) << (32-(shift)));
    x >>= shift;
  x += 128;
  if (x>255)
    return 255;
  else if (x<0)
    return 0;
    return x;
}
void
tinyjpeg_idct_float (struct component *compptr, uint8_t *output_buf, int stride)
{
  float tmp0, tmp1, tmp2, tmp3, tmp4, tmp5, tmp6, tmp7;
  float tmp10, tmp11, tmp12, tmp13;
  float z5, z10, z11, z12, z13;
  int16_t *inptr;
  float *quantptr;
  float *wsptr;
  uint8_t *outptr;
  int ctr;
  float workspace[(8*8)];
  quantptr = compptr->Q_table;
  wsptr = workspace;
  for (ctr = 8; ctr > 0; ctr--) {
    if (inptr[8*1] == 0 && inptr[8*2] == 0 &&
 inptr[8*3] == 0 && inptr[8*4] == 0 &&
 inptr[8*5] == 0 && inptr[8*6] == 0 &&
 inptr[8*7] == 0) {
      float dcval = (((float) (inptr[8*0])) * (quantptr[8*0]));
      wsptr[8*0] = dcval;
      wsptr[8*1] = dcval;
      wsptr[8*2] = dcval;
      wsptr[8*3] = dcval;
      wsptr[8*4] = dcval;
      wsptr[8*5] = dcval;
      wsptr[8*6] = dcval;
      wsptr[8*7] = dcval;
      inptr++;
      quantptr++;
      wsptr++;
      continue;
    }
    tmp0 = (((float) (inptr[8*0])) * (quantptr[8*0]));
    tmp1 = (((float) (inptr[8*2])) * (quantptr[8*2]));
    tmp2 = (((float) (inptr[8*4])) * (quantptr[8*4]));
    tmp3 = (((float) (inptr[8*6])) * (quantptr[8*6]));
    tmp10 = tmp0 + tmp2;
    tmp11 = tmp0 - tmp2;
    tmp13 = tmp1 + tmp3;
    tmp12 = (tmp1 - tmp3) * ((float) 1.414213562) - tmp13;
    tmp0 = tmp10 + tmp13;
    tmp3 = tmp10 - tmp13;
    tmp1 = tmp11 + tmp12;
    tmp2 = tmp11 - tmp12;
    tmp4 = (((float) (inptr[8*1])) * (quantptr[8*1]));
    tmp5 = (((float) (inptr[8*3])) * (quantptr[8*3]));
    tmp6 = (((float) (inptr[8*5])) * (quantptr[8*5]));
    tmp7 = (((float) (inptr[8*7])) * (quantptr[8*7]));
    z13 = tmp6 + tmp5;
    z10 = tmp6 - tmp5;
    z11 = tmp4 + tmp7;
    z12 = tmp4 - tmp7;
    tmp7 = z11 + z13;
    tmp11 = (z11 - z13) * ((float) 1.414213562);
    z5 = (z10 + z12) * ((float) 1.847759065);
    tmp10 = ((float) 1.082392200) * z12 - z5;
    tmp12 = ((float) -2.613125930) * z10 + z5;
    tmp6 = tmp12 - tmp7;
    tmp5 = tmp11 - tmp6;
    tmp4 = tmp10 + tmp5;
    wsptr[8*0] = tmp0 + tmp7;
    wsptr[8*7] = tmp0 - tmp7;
    wsptr[8*1] = tmp1 + tmp6;
    wsptr[8*2] = tmp2 + tmp5;
    wsptr[8*5] = tmp2 - tmp5;
    wsptr[8*4] = tmp3 + tmp4;
    wsptr[8*3] = tmp3 - tmp4;
    inptr++;
    quantptr++;
    wsptr++;
  }
  for (ctr = 0; ctr < 8; ctr++) {
    tmp11 = wsptr[0] - wsptr[4];
    tmp12 = (wsptr[2] - wsptr[6]) * ((float) 1.414213562) - tmp13;
    tmp0 = tmp10 + tmp13;
    tmp1 = tmp11 + tmp12;
    z10 = wsptr[5] - wsptr[3];
    tmp12 = ((float) -2.613125930) * z10 + z5;
    tmp6 = tmp12 - tmp7;
    outptr[0] = descale_and_clamp((int)(tmp0 + tmp7), 3);
    outptr[7] = descale_and_clamp((int)(tmp0 - tmp7), 3);
    outptr[1] = descale_and_clamp((int)(tmp1 + tmp6), 3);
    outptr[6] = descale_and_clamp((int)(tmp1 - tmp6), 3);
    outptr[2] = descale_and_clamp((int)(tmp2 + tmp5), 3);
    outptr += stride;
  }
}
