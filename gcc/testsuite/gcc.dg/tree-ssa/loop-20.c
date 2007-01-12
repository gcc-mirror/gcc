/* PR tree-optimization/29516 */

/* { dg-do compile { target fpic } } */
/* { dg-options "-O -ftree-vrp -fPIC -fdump-tree-ivopts" } */

typedef struct gfc_se { int pre; } gfc_se;
typedef struct gfc_ss_info { int dim[7]; } gfc_ss_info;
int gfc_rank_cst[7 + 1];
gfc_conv_array_transpose (gfc_se * se) {
  int dest, src, dest_index, src_index;
  gfc_ss_info *dest_info;
  int n;
  for (n = 0; n < 2; n++) {
    dest_info->dim[n] = n;
    src_index = gfc_rank_cst[1 - n];
    a (se->pre, b (dest, dest_index), c (src, src_index));
  }
}

/* Ivopts should not produce multiplication by a pointer constant.  */

/* { dg-final { scan-tree-dump-times "\\* \[0-9\]*B;" 0 "ivopts" } } */
/* { dg-final { cleanup-tree-dump "ivopts" } } */
