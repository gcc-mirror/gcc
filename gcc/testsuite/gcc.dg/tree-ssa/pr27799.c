/* { dg-do link } */
/* { dg-options "-O2" } */

struct X { double m; int x; };
struct Y { int y; short d; };
struct YY { int y; short d; char c; };

extern void link_error (void);

int foo(struct X *x,  struct Y *y)
{
  x->x =  0;
  y->y =  1;
  if (x->x != 0)
    link_error ();
}

int foo_no(struct X *x,  struct YY *y)
{
  x->x =  0;
  y->y =  1;
  if (x->x != 0)
    link_error ();
}

int main() {}
