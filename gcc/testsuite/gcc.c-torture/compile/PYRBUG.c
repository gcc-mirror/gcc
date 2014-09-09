typedef struct
{
  int v;
  int h;
} Point;

typedef struct
{
  int top, left, bottom, right;
} Rect;

int
x_PtInRect (Point pt, Rect *r)
{
  return  pt.v >= r->top  && pt.v < r->bottom
    && pt.h >= r->left && pt.h < r->right;
}
