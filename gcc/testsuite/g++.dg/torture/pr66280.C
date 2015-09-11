// { dg-do compile }

typedef struct
{
  short re;
  short im;
} cint16_T;
typedef struct
{
  int re;
  int im;
} cint32_T;
int a;
short b;
cint16_T *c;
cint32_T *d, *e;
void
fn1 ()
{
  for (; a; a++)
    {
      d[a].re = d[a].im = e[a].re = c[a].re * b;
      e[a].im = c[a].im * b;
    }
}
