// Bug: g++ dies on this input.
// Build don't link:

inline char abs (char x) { return 0; }

extern "C" {
  inline int abs (int x) { return 1; }
}
