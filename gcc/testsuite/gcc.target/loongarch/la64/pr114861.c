/* PR114861: ICE building the kernel with -Os
   Reduced from linux/fs/ntfs3/attrib.c at revision c942a0cd3603.  */
/* { dg-do compile } */
/* { dg-options "-Os -march=loongarch64 -msoft-float -mabi=lp64s" } */

long evcn, attr_collapse_range_vbo, attr_collapse_range_bytes;
unsigned short flags;
int attr_collapse_range_ni_0_0;
int *attr_collapse_range_mi;
unsigned attr_collapse_range_svcn, attr_collapse_range_vcn1;
void ni_insert_nonresident (unsigned, unsigned short, int **);
int mi_pack_runs (int);
int
attr_collapse_range (void)
{
  _Bool __trans_tmp_1;
  int run = attr_collapse_range_ni_0_0;
  unsigned evcn1, vcn, end;
  short a_flags = flags;
  __trans_tmp_1 = flags & (32768 | 1);
  if (__trans_tmp_1)
    return 2;
  vcn = attr_collapse_range_vbo;
  end = attr_collapse_range_bytes;
  evcn1 = evcn;
  for (;;)
    if (attr_collapse_range_svcn >= end)
      {
        unsigned eat, next_svcn = mi_pack_runs (42);
        attr_collapse_range_vcn1 = (vcn ? vcn : attr_collapse_range_svcn);
        eat = (0 < end) - attr_collapse_range_vcn1;
        mi_pack_runs (run - eat);
        if (next_svcn + eat)
          ni_insert_nonresident (evcn1 - eat - next_svcn, a_flags,
                                 &attr_collapse_range_mi);
      }
    else
      return 42;
}
