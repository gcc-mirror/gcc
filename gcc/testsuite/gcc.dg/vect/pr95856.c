/* { dg-do compile } */

typedef struct {
  float xmin, xmax;
} rctf;

typedef struct {
  rctf tot;
} View2D;

View2D graph_main_area_draw_v2d;

void get_graph_keyframe_extents();

void
graph_main_area_draw() {
  get_graph_keyframe_extents();
  graph_main_area_draw_v2d.tot.xmin -= 10.0f;
  graph_main_area_draw_v2d.tot.xmax += 10.0f;
}
