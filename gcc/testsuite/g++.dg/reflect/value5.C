// PR c++/125820
// P4101R1, Consteval-only Values for C++26
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^::);

struct S
{
  info i;
  int n;
};

constexpr info arr[] = { ^^int, ^^double, ^^char };
constexpr S s = { ^^int, 42 };
constexpr S sarr[2] = { { ^^int, 1 }, { ^^double, 2 } };
constexpr const info *p = arr;

// These check check_out_of_consteval_use which uses a tree walk
// on its expr.  value6.C checks consteval_only_p not used in
// a tree walk.
const void *vp = &p;		  // { dg-error "initialized with a consteval-only value" }

const auto p1 = &arr[0];	  // { dg-error "initialized with a consteval-only value" }
const auto p2 = arr;	  // { dg-error "initialized with a consteval-only value" }
const auto p3 = arr + 1;	  // { dg-error "initialized with a consteval-only value" }
const auto p4 = arr + 3;	  // { dg-error "initialized with a consteval-only value" }
const auto p5 = &s;		  // { dg-error "initialized with a consteval-only value" }
const auto p6 = &s.i;	  // { dg-error "initialized with a consteval-only value" }
const auto p7 = &sarr[1];	  // { dg-error "initialized with a consteval-only value" }
const auto p8 = &sarr[1].i;	  // { dg-error "initialized with a consteval-only value" }

struct Holder { const info *p; int n; };
auto h = Holder{ &arr[0], 1 };	  // { dg-error "initialized with a consteval-only value" }

constexpr bool cond = true;
const auto p_cond = cond ? &arr[0] : &arr[1];	  // { dg-error "initialized with a consteval-only value" }

// Not an error.
auto pdm = &S::i;
