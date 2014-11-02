/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-Os -fselective-scheduling2 -fsel-sched-pipelining -fprofile-generate -fno-early-inlining" } */

static void bmp_iter_next (int *bi)
{
  *bi >>= 1;
}

int bmp_iter_set (int *, int);
void bitmap_clear (void);
void bitmap_initialize_stat (void);

void df_md_alloc (int bi, int bb_index, int bb_info)
{
  for (; bmp_iter_set (&bi, bb_index); bmp_iter_next (&bi))
    if (bb_info)
      bitmap_clear ();
    else
      bitmap_initialize_stat ();
}
