// { dg-do assemble  }
// Origin: Neil Booth, from PR #66

extern "C"
{
  class foo
  {
  public:
    ~foo ();
    void bar (foo *);
    foo ();
  };
}

