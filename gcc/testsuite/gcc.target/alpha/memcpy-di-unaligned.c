/* { dg-do run } */
/* { dg-additional-sources memcpy-di-aligned.c } */
/* { dg-additional-sources memcpy-di-unaligned-src.c } */
/* { dg-additional-sources memcpy-di-unaligned-dst.c } */
/* { dg-options "" } */

void memcpy_aligned_data_di (void);
void memcpy_unaligned_dst_di (void *);
void memcpy_unaligned_src_di (const void *);

extern unsigned long aligned_src_di[];
extern unsigned long aligned_dst_di[];
extern unsigned long unaligned_src_di[];
extern unsigned long unaligned_dst_di[];

int
main (void)
{
  unsigned long v;
  int i;

  for (i = 1, v = 0x0807060504030201; i < 8; i++, v += 0x0808080808080808)
    unaligned_src_di[i] = v;
  asm ("" : : : "memory");
  memcpy_unaligned_dst_di (aligned_src_di + 1);
  asm ("" : : : "memory");
  memcpy_aligned_data_di ();
  asm ("" : : : "memory");
  memcpy_unaligned_src_di (aligned_dst_di + 1);
  asm ("" : : : "memory");
  for (i = 1, v = 0x0807060504030201; i < 8; i++, v += 0x0808080808080808)
    if (unaligned_dst_di[i] != v)
      return 1;
  if (unaligned_src_di[0] != 0xfefdfcfbfaf9f8f7)
      return 1;
  if (unaligned_src_di[8] != 0xfefdfcfbfaf9f8f7)
      return 1;
  if (aligned_src_di[0] != 0xe6e7e8e9eaebeced)
      return 1;
  if (aligned_src_di[8] != 0xe6e7e8e9eaebeced)
      return 1;
  if (aligned_dst_di[0] != 0xdcdbdad9d8d7d6d5)
      return 1;
  if (aligned_dst_di[8] != 0xdcdbdad9d8d7d6d5)
      return 1;
  if (unaligned_dst_di[0] != 0xc4c5c6c7c8c9cacb)
      return 1;
  if (unaligned_dst_di[8] != 0xc4c5c6c7c8c9cacb)
      return 1;
  return 0;
}
