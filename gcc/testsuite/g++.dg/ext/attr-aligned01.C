// PR c++/48138
// { dg-options -std=c++0x }

#define ALIGNED(x) __attribute__((aligned(x)))
#define SA(X) static_assert ((X),#X)

template<typename T>
void type_alignment(const T&) {
  struct { char c; T t; } s;
  SA((char*)&s.t - (char*)&s.c == 8);
}

int main() {
  typedef char unaligned[15];
  typedef char aligned[15] ALIGNED(8);

  aligned z;
  type_alignment(z);
  type_alignment<unaligned ALIGNED(8)>(z);
}
