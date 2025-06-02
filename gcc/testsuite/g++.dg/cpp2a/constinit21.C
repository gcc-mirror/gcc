// PR c++/120506
// { dg-do compile { target c++20 } }
// Test that we give more information about why the init is non-constant

struct A
{
  constexpr A(int c) : counter(c) { }

  int counter;
};


struct B : A
{
  constexpr B(int c) : A(c) { }

  int i; // OOPS, not initialized
};

struct C
{
  B sem;

  constexpr C(int c) : sem(c) { }
};

constinit C s(0);		// { dg-error "incompletely initialized" }
// { dg-prune-output "constant" }
