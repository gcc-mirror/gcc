/* { dg-do compile } */

int get_prev_frame_segid(unsigned char *p, int n)
{
  int tem;
  unsigned seg_id = 8;
  for (int x = 0; x < n; x++)
    {
      int a = seg_id;
      tem = a < p[x] ? a : p[x];
      seg_id = tem;
    }
  return tem;
}

/* { dg-final { scan-tree-dump "optimized: loop vectorized" "vect" { target { vect_int && { ! vect_no_int_min_max } } } } } */
