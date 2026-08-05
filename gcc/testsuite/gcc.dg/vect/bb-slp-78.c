/* { dg-do compile } */
/* { dg-additional-options "-march=x86-64-v2" { target { x86_64-*-* i?86-*-* } } } */

typedef struct {
  float xmin, xmax;
  float ymin, ymax;
} rctf;
short U_0;
int node_find_indicated_socket_in_out;
void BLI_rctf_isect_pt(rctf *);
void node_find_indicated_socket(float cursor[])
{
  rctf rect;
  rect.xmin = rect.ymin = cursor[1] - 4;
  rect.xmax = rect.ymax = cursor[1] + 0;
  if (node_find_indicated_socket_in_out)
    rect.xmax += rect.xmin -= U_0;
  BLI_rctf_isect_pt(&rect);
}
