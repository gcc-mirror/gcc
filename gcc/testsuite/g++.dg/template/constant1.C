// PR c++/49855

extern void foo(int);

template <class Key, class Value> void Basic() {
  const int kT = 1.5e6;        // <--- causes ICE
  int size = kT*2/3;
  do {
    foo(size);
    size = size * 0.5 - 1;
  } while (size >= 0 );

}
