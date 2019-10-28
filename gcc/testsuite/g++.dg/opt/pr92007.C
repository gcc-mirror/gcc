// PR rtl-optimization/92007
// { dg-do compile }
// { dg-options "-O2 -fno-tree-dominator-opts -fno-tree-forwprop --param max-cse-insns=0 -Wno-return-type -std=gnu++98 -freorder-blocks-and-partition" }

void
sb (int *);

class d4 {
public:
  ~d4();
  void gb ();
  int op () { return no; }
  int wl () { return tf; }
  bool ee () try { gb (); } catch (...) { return false; }
  bool b1 () { return (tf == no) ? false : ee (); }

private:
  int no, tf;
};

void
hs (int *v9)
{
  d4 p6;

  p6.gb ();
  if (p6.op () > p6.wl ())
    {
      p6.b1 ();
      sb (v9);
    }
}
