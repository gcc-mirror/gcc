/* { dg-do run } */
/* { dg-additional-sources memcpy-si-aligned.c } */
/* { dg-additional-sources memcpy-si-unaligned-src.c } */
/* { dg-additional-sources memcpy-si-unaligned-dst.c } */
/* { dg-options "" } */

void memcpy_aligned_data_si (void);
void memcpy_unaligned_dst_si (void *);
void memcpy_unaligned_src_si (const void *);

extern unsigned int aligned_src_si[];
extern unsigned int aligned_dst_si[];
extern unsigned int unaligned_src_si[];
extern unsigned int unaligned_dst_si[];

int
main (void)
{
  unsigned int v;
  int i;

  for (i = 1, v = 0x04030201; i < 16; i++, v += 0x04040404)
    unaligned_src_si[i] = v;
  asm ("" : : : "memory");
  memcpy_unaligned_dst_si (aligned_src_si + 1);
  asm ("" : : : "memory");
  memcpy_aligned_data_si ();
  asm ("" : : : "memory");
  memcpy_unaligned_src_si (aligned_dst_si + 1);
  asm ("" : : : "memory");
  for (i = 1, v = 0x04030201; i < 16; i++, v += 0x04040404)
    if (unaligned_dst_si[i] != v)
      return 1;
  if (unaligned_src_si[0] != 0xfefdfcfb)
      return 1;
  if (unaligned_src_si[16] != 0xfefdfcfb)
      return 1;
  if (aligned_src_si[0] != 0xeaebeced)
      return 1;
  if (aligned_src_si[16] != 0xeaebeced)
      return 1;
  if (aligned_dst_si[0] != 0xdcdbdad9)
      return 1;
  if (aligned_dst_si[16] != 0xdcdbdad9)
      return 1;
  if (unaligned_dst_si[0] != 0xc8c9cacb)
      return 1;
  if (unaligned_dst_si[16] != 0xc8c9cacb)
      return 1;
  return 0;
}
