/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */

typedef struct {
  float xmin, xmax;
} rctf;
int U_0;
float BLI_rctf_size_x_rct_1, view_zoomdrag_apply_dx;
void *view_zoomdrag_apply_op_0;
float RNA_float_get();
typedef struct {
  rctf cur;
} View2D;
typedef struct {
  View2D v2d;
} v2dViewZoomData;
void view_zoomdrag_apply() {
  v2dViewZoomData *vzd = view_zoomdrag_apply_op_0;
  View2D *v2d = &vzd->v2d;
  view_zoomdrag_apply_dx = RNA_float_get();
  if (U_0) {
    float mval_fac = BLI_rctf_size_x_rct_1, mval_faci = mval_fac,
          ofs = mval_faci * view_zoomdrag_apply_dx;
    v2d->cur.xmin += ofs + view_zoomdrag_apply_dx;
    v2d->cur.xmax += ofs - view_zoomdrag_apply_dx;
  } else {
    v2d->cur.xmin += view_zoomdrag_apply_dx;
    v2d->cur.xmax -= view_zoomdrag_apply_dx;
  }
}
