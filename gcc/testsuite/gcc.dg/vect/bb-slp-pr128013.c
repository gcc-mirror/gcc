/* { dg-do compile } */

typedef int v4si __attribute__ ((vector_size (16)));
typedef float v4sf __attribute__ ((vector_size (16)));
extern int rt (void) __attribute__ ((returns_twice));
extern void bar (void);
extern void use (v4si);
extern void usef (v4sf);

/* The vector store goes before the last scalar store.  */

void
f1 (int *out, v4si v, int c)
{
  int a0 = v[0], a1 = v[1], a2 = v[2], a3 = v[3];
  bar ();
  v = v * 3;
  if (c)
    {
      out[0] = a0;
      out[1] = a1;
      out[2] = a2;
      out[3] = a3;
    }
  bar ();
  rt ();
  use (v);
}

void
f2 (float *out, v4sf v)
{
  float a0 = v[0], a1 = v[1], a2 = v[2], a3 = v[3];
  bar ();
  v = v * 3;
  bar ();
  out[0] = a0 + 1;
  out[1] = a1 + 2;
  out[2] = a2 + 3;
  out[3] = a3 + 4;
  bar ();
  rt ();
  usef (v);
}

/* The reduction code goes at the root stmt.  */

int
f3 (v4si v, int c)
{
  int a0 = v[0], a1 = v[1], a2 = v[2], a3 = v[3];
  int r = 0;
  bar ();
  v = v * 3;
  if (c)
    r = a0 + a1 + a2 + a3;
  bar ();
  rt ();
  use (v);
  return r;
}
