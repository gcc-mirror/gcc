/* { dg-do compile { target powerpc*-*-* ia64-*-* i?86-*-* x86_64-*-* } } */
/* { dg-options "-O3 -fselective-scheduling2 -fsel-sched-pipelining" } */
typedef unsigned short int uint16_t;
__extension__ typedef __UINTPTR_TYPE__ uintptr_t;
typedef struct GFX_VTABLE
{
  int color_depth;
  unsigned char *line[];
}
BITMAP;
extern int _drawing_mode;
extern BITMAP *_drawing_pattern;
extern int _drawing_y_anchor;
extern unsigned int _drawing_x_mask;
extern unsigned int _drawing_y_mask;
extern uintptr_t bmp_write_line (BITMAP *, int);
  void
_linear_hline15 (BITMAP * dst, int dx1, int dy, int dx2, int color)
{
  int w;
  if (_drawing_mode == 0)
  {
    int x, curw;
    unsigned short *sline =
      (unsigned short *) (_drawing_pattern->
          line[((dy) -
            _drawing_y_anchor) & _drawing_y_mask]);
    unsigned short *s;
    unsigned short *d =
      ((unsigned short *) (bmp_write_line (dst, dy)) + (dx1));
    s = ((unsigned short *) (sline) + (x));
    if (_drawing_mode == 2)
    {
    }
    else if (_drawing_mode == 3)
    {
      do
      {
        w -= curw;
        do
        {
          unsigned long c = (*(s));
          if (!((unsigned long) (c) == 0x7C1F))
          {
            (*((uint16_t *) ((uintptr_t) (d))) = ((color)));
          }
          ((s)++);
        }
        while (--curw > 0);
        s = sline;
        curw =
          (((w) <
            ((int) _drawing_x_mask +
             1)) ? (w) : ((int) _drawing_x_mask + 1));
      }
      while (curw > 0);
    }
  }
}
