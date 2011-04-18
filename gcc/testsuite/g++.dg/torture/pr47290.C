// PR tree-optimization/47290
// { dg-do compile }

struct V
{
  V (int = 0);
  ~V ()
  {
    for (;;)
      ;
  }
  int size ();
};

struct S
{
  V a, b;
  S () : b (a.size ()) {}
} s;
