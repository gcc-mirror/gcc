// Build don't link:
// Origin: Mark Mitchell <mark@codesourcery.com>

template <class T>
void f (T t) {
  const_cast<T>(t);
}
