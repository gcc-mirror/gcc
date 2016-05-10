/* { dg-do compile } */

struct wv
{
  int qi;
} qp, *ft;
void *pb;

void
wz (void)
{
  struct wv *vf = pb ? (struct wv *)&pb : &qp;
  *ft = *vf;
}
