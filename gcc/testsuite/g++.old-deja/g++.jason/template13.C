// Bug: g++ fails to actually instantiate templates to the specifications of
// guiding decls.
// Special g++ Options: -g -ansi -pedantic-errors -fguiding-decls

template <class T> inline T min (T a, T b) { return a<b?a:b; }
double min (double, double);

int main () {
  return (int) min (0, 1.0);
}
