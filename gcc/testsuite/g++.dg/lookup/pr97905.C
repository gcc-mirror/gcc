// PR 97905


template <typename> void a() {
  extern int *b; // This decl gets an (unneeded) decl-lang-specific
}
int *b; // this does not
