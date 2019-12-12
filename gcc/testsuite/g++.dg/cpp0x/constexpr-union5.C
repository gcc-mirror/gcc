// PR c++/54922
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

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
  struct {		        // { dg-warning "10:ISO C\\+\\+ prohibits anonymous struct" }
    int h;
    struct {			// { dg-warning "12:ISO C\\+\\+ prohibits anonymous struct" }
      union {
	unsigned char i;
	int j;
      };
      int k;
    };
  };
  int l;

  constexpr B(): h(1), i(2), k(3), l(4) {}
};

constexpr B b;
SA((b.h == 1 && b.i == 2 && b.k == 3 && b.l == 4));
