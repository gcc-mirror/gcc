// Build don't link:

struct outer {
  template <class T> struct inner;
} o;
template <class T> struct outer::inner {};
