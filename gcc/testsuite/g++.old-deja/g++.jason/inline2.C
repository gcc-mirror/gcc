// Bug: the lang-specific bits of the decl for g aren't being copied when
// inlining.
// Special g++ Options: -O
// Build don't link:

inline void f () {
  void g ();
}

void h() {
  f();				// causes compiler segfault - 
}
