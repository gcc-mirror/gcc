/* PR optimization/11634 */

/* The following code used to ICE in verify_local_live_at_start on
   PA when compiled with -O2.  The cause was that split_all_insns was
   not updating liveness information when deleting no-op moves that
   had REG_UNUSED notes.  */

/* { dg-do compile { target hppa*-*-* } } */
/* { dg-options "-O2" } */

void *f(void *s);
void H5T_conv_vlen (unsigned long long nelmts, unsigned char *bg_ptr)
{
  long long seq_len;
  unsigned long long bg_seq_len = 0;
  unsigned src_base_size, dst_base_size;
  void *tmp_buf = 0;
  unsigned tmp_buf_size = 0;
  unsigned long long elmtno;
  for (elmtno = 0; elmtno < nelmts; elmtno++)
    {
      unsigned char *tmp = bg_ptr;
      bg_seq_len = *tmp;
      if (bg_seq_len > 0
          && tmp_buf_size <
          (unsigned) (bg_seq_len *
                      (src_base_size > dst_base_size
                       ? src_base_size
		       : dst_base_size)))
	{
	  tmp_buf_size =
	    (unsigned) (bg_seq_len *
			(src_base_size > dst_base_size
			 ? src_base_size
			 : dst_base_size));
	}
      if (bg_seq_len < seq_len)
	f ((unsigned char *) tmp_buf + dst_base_size * bg_seq_len);
    }
}

