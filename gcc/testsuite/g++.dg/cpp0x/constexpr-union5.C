// PR c++/54922
// { dg-options "-std=c++11 -pedantic" }

#define SA(X) static_assert(X,#X)

struct A
{
  union {
    union {
      union {
	unsigned char i;
	int j;
      };
    };
  };

  constexpr A() : i(42) {}
};

constexpr A a;
SA((a.i == 42));

struct B
{
  struct {
    int h;
    struct {
      union {
	unsigned char i;
	int j;
      };
      int k;
    };				// { dg-warning "anonymous struct" }
  };				// { dg-warning "anonymous struct" }
  int l;

  constexpr B(): h(1), i(2), k(3), l(4) {}
};

constexpr B b;
SA((b.h == 1 && b.i == 2 && b.k == 3 && b.l == 4));
