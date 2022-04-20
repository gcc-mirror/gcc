// PR c++/97219
// { dg-do compile { target c++14 } }

struct B;

template <typename T>
auto f(T *) {
  void q(B *, void * = static_cast<T *>(0));
  return [](auto *p) { q(p); };
}

void q(void *) = delete;

int main(void) {
  B *bp = 0;
  f(bp)(bp);
}
