// PR c++/58882
// { dg-do compile { target c++11 } }
// { dg-options "-pedantic" }

struct A
{
  constexpr operator int() const { return 0; }
};

int a[] = { [A()] = 0 };	// { dg-warning "does not allow C99 designated initializers" }

enum E { e0 };

struct B
{
  constexpr operator E() const { return E::e0; }
};

int b[] = { [B()] = 0 };	// { dg-warning "does not allow C99 designated initializers" }

enum class SE { se0 };

struct C
{
  constexpr operator SE() const { return SE::se0; }
};

int c[] = { [C()] = 0 }; // { dg-error "integral constant-expression" }
			 // { dg-warning "does not allow C99 designated initializers" "" { target *-*-* } .-1 }
