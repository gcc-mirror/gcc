// Build don't link:
// Origin: <Corey Kosak> kosak@cs.cmu.edu

struct moo {
  template<bool x> struct cow {};

  template<bool x>
  struct moo2 {
    void func(cow<x> &c) { }
  };
};
