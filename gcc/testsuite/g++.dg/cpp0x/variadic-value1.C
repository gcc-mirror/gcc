// PR c++/52796
// { dg-do run { target c++11 } }

typedef int int32_t __attribute__((mode (__SI__)));

inline void *operator new(__SIZE_TYPE__ s, void *p) { return p; }

struct A
{
  int32_t i;
  template<class... Ts>
  A(Ts&&... ts): i(ts...) { }
};

static union {
  unsigned char c[sizeof(A)];
  int32_t i;
};

int main()
{
  i = 0xdeadbeef;
  new(c) A;
  if (i != 0)
    __builtin_abort();
}
