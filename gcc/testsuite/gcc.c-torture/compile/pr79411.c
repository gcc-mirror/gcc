/* PR tree-optimization/79411 */

typedef struct __jmp_buf_tag { char buf[1024]; } jmp_buf[1];
extern int setjmp (jmp_buf);
extern int bar (unsigned int *);
extern jmp_buf *baz (void);
struct C { int c1; unsigned int c2, c3, c4; };

void
foo (struct C *x, const int *y, unsigned int *z, unsigned int e, unsigned int g)
{
  unsigned int d = 0;
  unsigned long f;
  setjmp (*baz ());
  f = 1 + d;
  if ((x->c1 || x->c2) && g && (!e || d >= 8))
    d = 16;
  else
    d = 8;
  if ((!x->c3 && !x->c4 || *y == 0) && !e && bar (z))
    *z = 1 + f;
}
