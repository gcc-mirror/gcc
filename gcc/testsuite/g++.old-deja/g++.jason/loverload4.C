// { dg-do assemble  }
// Bug: g++ dies on this input.

inline char abs (char x) { return 0; }

extern "C" {
  inline int abs (int x) { return 1; }
}
