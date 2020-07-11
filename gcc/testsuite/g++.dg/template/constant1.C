// PR c++/49855

extern void foo(int);

template <class Key, class Value> void Basic() {
#if __SIZEOF_INT__ == 4
  const int kT = 1.5e6;        // <--- causes ICE
#elif __SIZEOF_INT__ == 2
  const int kT = 1.5e4;        // <--- causes ICE
#elif __SIZEOF_INT__ == 1
  const int kT = 1.5e2;        // <--- causes ICE
#endif
  int size = kT*2/3;
  do {
    foo(size);
    size = size * 0.5 - 1;
  } while (size >= 0 );

}
