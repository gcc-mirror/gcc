// { dg-do assemble  }
// { dg-options "-O" }
// Bug: the lang-specific bits of the decl for g aren't being copied when
// inlining.

inline void f () {
  void g ();
}

void h() {
  f();				// causes compiler segfault - 
}
