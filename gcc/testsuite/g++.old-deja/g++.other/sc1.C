// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

void f() {
  int *i = 0;
  const int *c = 0;

  static_cast <const int *>(i);
  static_cast <int *>(c);  // ERROR - casts away constness
}
