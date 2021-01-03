/* { dg-do compile } */

struct {
  int x;
  int y;
} do_plasma_rect;

int do_plasma_context_0, do_plasma_x2, do_plasma_y2, do_plasma_plasma_depth,
    do_plasma_xm, do_plasma_ym;
void gegl_buffer_set();

void do_plasma(int x1, int y1) {
  if (__builtin_expect(({
                         int _g_boolean_var_;
                         if (do_plasma_context_0)
                           _g_boolean_var_ = 1;
                         else
                           _g_boolean_var_ = 0;
                         _g_boolean_var_;
                       }),
                       0)) {
    do_plasma_rect.x = x1;
    do_plasma_rect.y = y1;
    gegl_buffer_set();
  }
  do_plasma_xm = (x1 + do_plasma_x2) / 2;
  do_plasma_ym = (y1 + do_plasma_y2) / 2;
  if (do_plasma_plasma_depth) {
    do_plasma_rect.x = do_plasma_xm;
    do_plasma_rect.y = do_plasma_ym;
    return;
  }
  do_plasma(do_plasma_xm, do_plasma_ym);
}
