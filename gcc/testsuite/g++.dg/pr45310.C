/* { dg-do compile } */
/* { dg-options "-O1 -fnon-call-exceptions" }  */

static inline const int &
max (const int &a, const int &b)
{
  return a ? a : b;
}

static inline int
baz ()
{
  return max (0, 0);
}

struct S
{
  ~S ()
  {
    baz ();
  }
};

void bar ();
void
foo ()
{
  S s;
  bar ();
}

