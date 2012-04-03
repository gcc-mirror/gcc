// PR c++/52796
// { dg-do run { target c++11 } }

inline void *operator new(__SIZE_TYPE__ s, void *p) { return p; }

struct A
{
  int i;
  template<class... Ts>
  A(Ts&&... ts): i(ts...) { }
};

static union {
  unsigned char c[sizeof(A)];
  int i;
};

int main()
{
  i = 0xdeadbeef;
  new(c) A;
  if (i != 0)
    __builtin_abort();
}
