// PR c++/21336

typedef __SIZE_TYPE__ size_t;
template<class _T> void* operator new( size_t Size, _T&);
struct B {
  int a;
  int* m() {
    return new(a) int;
  }
};
B* n() {
  return new B();
}

