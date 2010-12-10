// PR tree-optimization/46864
// { dg-do compile }
// { dg-options "-O -fnon-call-exceptions" }

int baz ();

struct S
{
  int k;
  bool bar () throw ()
  {
    int m = baz ();
    for (int i = 0; i < m; i++)
      k = i;
    return m;
  }
};

extern S *s;

void
foo ()
{
  while (baz () && s->bar ())
    ;
}
