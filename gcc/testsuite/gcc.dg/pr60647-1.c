/* { dg-do compile } */
/* { dg-options "-O2" } */

struct _wincore
{
  int y;
  int width;
};
int a;
static fn1 (dpy, winInfo) struct _XDisplay *dpy;
struct _wincore *winInfo;
{
  a = winInfo->width;
  fn2 ();
}

static fn3 (dpy, winInfo, visrgn) struct _XDisplay *dpy;
{
  int b = fn1 (0, winInfo);
  fn4 (0, 0, visrgn);
}

fn5 (event) struct _XEvent *event;
{
  fn3 (0, 0, 0);
}
