/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "dont_delete" } } */

typedef struct { int x; } __attribute__((may_alias)) S;

extern void dont_delete (void);

void f(S *s, float *f)
{
  s->x = 1;
  *f = 0;
  if (s->x != 1)
    dont_delete ();
}
