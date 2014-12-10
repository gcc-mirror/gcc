typedef unsigned char uint8_t;
typedef unsigned int uint32_t;
static __attribute__ (( always_inline )) __inline__
void rop_8_notsrc_or_dst(uint8_t *dst, uint8_t src)
{
  *dst = (~(src)) | (*dst);
}
void cirrus_colorexpand_notsrc_or_dst_8 (uint8_t * dst, int bits)
{
  uint8_t src;
  uint32_t colors[2];
  src = colors[bits];
  rop_8_notsrc_or_dst(dst, src);
}
