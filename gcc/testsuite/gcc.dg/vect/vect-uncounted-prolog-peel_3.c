/* { dg-add-options vect_early_break } */
/* { dg-do compile } */
/* { dg-require-effective-target vect_early_break } */
/* { dg-require-effective-target vect_int } */

struct {
  int segments[];
} seek_to_sample_coarse_f;

int seek_to_sample_coarse_i, seek_to_sample_coarse_f_1;

void seek_to_sample_coarse() {
  int end_pos = seek_to_sample_coarse_f_1;
  for (;;) {
    seek_to_sample_coarse_i = end_pos;
    for (; end_pos > 0; --end_pos)
      if (seek_to_sample_coarse_f.segments[end_pos - 1])
        break;
    if (end_pos)
      break;
  }
}

/* { dg-final { scan-tree-dump {note:\s*Alignment of access forced using peeling.} "vect" } } */
/* { dg-final { scan-tree-dump {if \(ivtmp_[0-9_]+ >= prolog_loop_niters.[0-9_]+\)\n\s*goto} "vect" } } */
/* { dg-final { scan-tree-dump {vectorized 1 loops in function} "vect" } } */
