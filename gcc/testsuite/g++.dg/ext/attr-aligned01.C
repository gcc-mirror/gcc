// PR c++/48138

#define ALIGNED(x) __attribute__((aligned(x)))
#define SA(X) int ar[(X)?1:-1];

template<typename T>
void type_alignment(const T&) {
  struct { char c; T t; } s;
  SA((char*)&s.t - (char*)&s.c == 1);
}

template <class T> struct A { char c; T t; };

int main() {
  typedef char unaligned[15];
  typedef char aligned[15] ALIGNED(8);

  A<aligned> a;			// { dg-warning "ignoring attributes" }

  SA((char*)&a.t - (char*)&a.c == 1);

  aligned z;
  type_alignment(z);		// { dg-warning "ignoring attributes" "" { xfail *-*-* } }
  type_alignment<unaligned ALIGNED(8)>(z); // { dg-warning "ignoring attributes" "" { xfail *-*-* } }
}
