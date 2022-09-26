/* { dg-do compile } */
/* { dg-options "-march=skylake -Ofast" } */

typedef struct {
  float ymin, ymax;
} rctf;

rctf view2d_map_cur_using_maskUI_view2d_view_ortho_curmasked;
float view2d_map_cur_using_maskUI_view2d_view_ortho_yofs;

void BLI_rctf_translate();
void glLoadIdentity();

void
view2d_map_cur_using_maskUI_view2d_view_ortho() {
  BLI_rctf_translate(&view2d_map_cur_using_maskUI_view2d_view_ortho_curmasked);
  view2d_map_cur_using_maskUI_view2d_view_ortho_curmasked.ymin =
      __builtin_floor(view2d_map_cur_using_maskUI_view2d_view_ortho_curmasked.ymin) -
      view2d_map_cur_using_maskUI_view2d_view_ortho_yofs;
  view2d_map_cur_using_maskUI_view2d_view_ortho_curmasked.ymax =
      __builtin_floor(view2d_map_cur_using_maskUI_view2d_view_ortho_curmasked.ymax) -
      view2d_map_cur_using_maskUI_view2d_view_ortho_yofs;
  glLoadIdentity();
}
