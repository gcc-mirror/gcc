// PR ipa/127917
// { dg-do run }
// { dg-options "-O3" }

struct Selector {
  virtual void Accept () {}
};

struct Tree {
  Tree () : depth (2) {}
  virtual int Select (Selector &s) { Select (depth, s); return 1; }
  void Select (int d, Selector &s)
  {
    if (d)
      {
	Select (d - 1, s);
	Select (d - 1, s);
      }
    else
      s.Accept ();
  }
  int depth;
};

struct Bad : Selector {
  void Accept () { __builtin_abort (); }
};

__attribute__((noipa)) void
perform (Tree &t)
{
  Selector ok;
  if (t.depth != 1 && t.Select (ok))
    return;
  Bad bad;
  t.Select (bad);
  t.Select (bad);
}

int
main ()
{
  Tree t;
  perform (t);
}
