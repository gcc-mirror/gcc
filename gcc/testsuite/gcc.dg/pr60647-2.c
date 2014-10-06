/* { dg-do compile } */
/* { dg-options "-O2" } */

struct _wincore
{
  int width, height;
};

void fn1 (int);
void fn2 (int, int, int *);

static void
foo (void *dpy, struct _wincore *winInfo, int offset)
{
  fn1 (winInfo->height);
}

static void
bar (void *dpy, int winInfo, int *visrgn)
{
  ((void (*) (void *, int, int)) foo) ((void *) 0, winInfo, 0);  /* { dg-warning "function called through a non-compatible type" } */
  fn2 (0, 0, visrgn);
}

void
baz (void *dpy, int win, int prop)
{
  bar ((void *) 0, 0, (int *) 0);
}
