// PR c++/91877 - ICE with converting member of packed struct.
// { dg-do compile { target c++11 } }
// { dg-options "-fpack-struct" }

template <typename a> class b {
public:
  b(const a &);
};
struct {
  int *c;
} d;
void e() { b<const int *>(d.c); }
