/* { dg-do compile } */
/* { dg-options "-Wno-old-style-definition --param ipa-cp-value-list-size=0 -Os -fno-inline" } */

struct _wincore
{
  int y;
  int width;
};
int a;
void fn2 (void);
static int fn1 (dpy, winInfo) struct _XDisplay *dpy;
struct _wincore *winInfo;
{
  a = winInfo->width;
  fn2 ();
}

void fn4 (int, int, int);
static int fn3 (dpy, winInfo, visrgn) struct _XDisplay *dpy;
int winInfo, visrgn;
{
  int b = fn1 (0, winInfo);
  fn4 (0, 0, visrgn);
}

int
fn5 (event) struct _XEvent *event;
{
  fn3 (0, 0, 0);
}
