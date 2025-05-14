// PR c++/120012
// { dg-do compile { target c++11 } }
// { dg-additional-options "-fabi-version=21 -Wabi=20" }

struct A
{
   A(const A&) = default;
   A(A&&) = default;
   A& operator=(A&&) = default;
   unsigned int a;
   unsigned char b;
};
struct B: A
{
   unsigned char c;		// { dg-warning "offset" "" { target c++20 } }
};

static_assert(sizeof(A) == (2 * sizeof(unsigned int)), "");
static_assert(sizeof(B) == (3 * sizeof(unsigned int)), "");
