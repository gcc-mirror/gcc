// PR c++/104944
// { dg-do compile { target c++11 } }
// { dg-options "-Wpedantic" }

struct inc;

struct alignas(inc) S1 { }; // { dg-error "invalid application" }
struct alignas(void) S2 { }; // { dg-error "invalid application" }

template <typename T>
struct alignas(T) S4 {}; // { dg-error "invalid application" }

template <typename T>
struct alignas(T) S5 {}; // { dg-error "invalid application" }

S4<void> s1;
S5<inc> s2;

void
g ()
{
  auto s1 = alignof(void); // { dg-error "invalid application" }
  auto s2 = alignof(const void); // { dg-error "invalid application" }
  auto s3 = __alignof(void); // { dg-warning "invalid application" }
  auto s4 = alignof(inc); // { dg-error "invalid application" }
}
