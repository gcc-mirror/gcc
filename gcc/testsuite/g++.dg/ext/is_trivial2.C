// PR c++/58074
// { dg-do compile { target c++11 } }

struct Trivial
{
  Trivial() = delete;
};

struct NonTrivial
{
  NonTrivial() = default;
  NonTrivial(NonTrivial&) = default;
  NonTrivial& operator=(NonTrivial&) = default;
};

// As it happens, 58074 was originally asking for the opposite result
// of __is_trivial than we're checking here, so what was non-trivial
// was supposed to be trivial and vice versa.  But here we are.
static_assert(!__is_trivial(Trivial), "Ouch");
static_assert(__is_trivial(NonTrivial), "Ouch");
