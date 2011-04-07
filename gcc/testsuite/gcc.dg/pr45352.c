/* { dg-do compile { target powerpc*-*-* ia64-*-* x86_64-*-* } } */
/* { dg-options "-Os -fselective-scheduling2 -fsel-sched-pipelining -fprofile-generate" } */

static inline void
bmp_iter_next (int *bi, int *bit_no)
{
  *bi >>= 1;
  *bit_no += 1;
}

int bmp_iter_set (int *bi, int *bit_no);
void bitmap_initialize_stat (int, ...);
void bitmap_clear (void);

void
df_md_alloc (int bi, int bb_index, void *bb_info)
{
  for (; bmp_iter_set (&bi, &bb_index); bmp_iter_next (&bi, &bb_index))

    if (bb_info)
      bitmap_clear ();
    else
      bitmap_initialize_stat (0);
}
