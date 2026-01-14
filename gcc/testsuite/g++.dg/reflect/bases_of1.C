// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::bases_of and has_inaccessible_bases.

#include <meta>

using namespace std::meta;

struct A { int a; };
struct B { int b; };
struct C { int c; };
struct D { int d; };
struct E { int e; };
struct F { int f; };
struct G { int g; };
struct H { int h; };
struct I : public A, protected B, private C { int i; };
struct J : protected D, private E, public F { int j; };
struct K : public G, protected I, private H, public J { int k; };
struct L : virtual A { int l; };
struct M : virtual A { int m; };
struct N : public L, protected M, public virtual A { int n; };

constexpr access_context gctx = access_context::current ();
constexpr access_context uctx = access_context::unchecked ();

static_assert (bases_of (^^A, uctx).size () == 0);
static_assert (bases_of (^^B, uctx).size () == 0);
static_assert (bases_of (^^C, uctx).size () == 0);
static_assert (!has_inaccessible_bases (^^C, uctx));
static_assert (bases_of (^^D, uctx).size () == 0);
static_assert (bases_of (^^E, uctx).size () == 0);
static_assert (bases_of (^^F, uctx).size () == 0);
static_assert (bases_of (^^G, uctx).size () == 0);
static_assert (bases_of (^^H, uctx).size () == 0);
static_assert (bases_of (^^I, uctx).size () == 3);
static_assert (is_base (bases_of (^^I, uctx)[0]));
static_assert (parent_of (bases_of (^^I, uctx)[0]) == ^^I);
static_assert (type_of (bases_of (^^I, uctx)[0]) == ^^A);
static_assert (identifier_of (bases_of (^^I, uctx)[0]) == "A");
static_assert (is_accessible (bases_of (^^I, uctx)[0], gctx));
static_assert (size_of (bases_of (^^I, uctx)[0]) == sizeof (A));
static_assert (offset_of (bases_of (^^I, uctx)[0]).total_bits () == 0);
static_assert (is_base (bases_of (^^I, uctx)[1]));
static_assert (parent_of (bases_of (^^I, uctx)[1]) == ^^I);
static_assert (type_of (bases_of (^^I, uctx)[1]) == ^^B);
static_assert (identifier_of (bases_of (^^I, uctx)[1]) == "B");
static_assert (!is_accessible (bases_of (^^I, uctx)[1], gctx));
static_assert (size_of (bases_of (^^I, uctx)[1]) == sizeof (B));
static_assert (offset_of (bases_of (^^I, uctx)[1]).total_bits () == __CHAR_BIT__ * sizeof (A));
static_assert (is_base (bases_of (^^I, uctx)[2]));
static_assert (parent_of (bases_of (^^I, uctx)[2]) == ^^I);
static_assert (type_of (bases_of (^^I, uctx)[2]) == ^^C);
static_assert (identifier_of (bases_of (^^I, uctx)[2]) == "C");
static_assert (!is_accessible (bases_of (^^I, uctx)[2], gctx));
static_assert (size_of (bases_of (^^I, uctx)[2]) == sizeof (C));
static_assert (offset_of (bases_of (^^I, uctx)[2]).total_bits () == __CHAR_BIT__ * (sizeof (A) + sizeof (B)));
static_assert (!has_inaccessible_bases (^^I, uctx));
static_assert (bases_of (^^J, uctx).size () == 3);
static_assert (is_base (bases_of (^^J, uctx)[0]));
static_assert (parent_of (bases_of (^^J, uctx)[0]) == ^^J);
static_assert (type_of (bases_of (^^J, uctx)[0]) == ^^D);
static_assert (identifier_of (bases_of (^^J, uctx)[0]) == "D");
static_assert (!is_accessible (bases_of (^^J, uctx)[0], gctx));
static_assert (size_of (bases_of (^^J, uctx)[0]) == sizeof (D));
static_assert (offset_of (bases_of (^^J, uctx)[0]).total_bits () == 0);
static_assert (is_base (bases_of (^^J, uctx)[1]));
static_assert (parent_of (bases_of (^^J, uctx)[1]) == ^^J);
static_assert (type_of (bases_of (^^J, uctx)[1]) == ^^E);
static_assert (identifier_of (bases_of (^^J, uctx)[1]) == "E");
static_assert (!is_accessible (bases_of (^^J, uctx)[1], gctx));
static_assert (size_of (bases_of (^^J, uctx)[1]) == sizeof (E));
static_assert (offset_of (bases_of (^^J, uctx)[1]).total_bits () == __CHAR_BIT__ * sizeof (D));
static_assert (is_base (bases_of (^^J, uctx)[2]));
static_assert (parent_of (bases_of (^^J, uctx)[2]) == ^^J);
static_assert (type_of (bases_of (^^J, uctx)[2]) == ^^F);
static_assert (identifier_of (bases_of (^^J, uctx)[2]) == "F");
static_assert (is_accessible (bases_of (^^J, uctx)[2], gctx));
static_assert (size_of (bases_of (^^J, uctx)[2]) == sizeof (F));
static_assert (offset_of (bases_of (^^J, uctx)[2]).total_bits () == __CHAR_BIT__ * (sizeof (D) + sizeof (E)));
static_assert (!has_inaccessible_bases (^^J, uctx));
static_assert (bases_of (^^K, uctx).size () == 4);
static_assert (is_base (bases_of (^^K, uctx)[0]));
static_assert (parent_of (bases_of (^^K, uctx)[0]) == ^^K);
static_assert (type_of (bases_of (^^K, uctx)[0]) == ^^G);
static_assert (identifier_of (bases_of (^^K, uctx)[0]) == "G");
static_assert (is_accessible (bases_of (^^K, uctx)[0], gctx));
static_assert (size_of (bases_of (^^K, uctx)[0]) == sizeof (G));
static_assert (offset_of (bases_of (^^K, uctx)[0]).total_bits () == 0);
static_assert (is_base (bases_of (^^K, uctx)[1]));
static_assert (parent_of (bases_of (^^K, uctx)[1]) == ^^K);
static_assert (type_of (bases_of (^^K, uctx)[1]) == ^^I);
static_assert (identifier_of (bases_of (^^K, uctx)[1]) == "I");
static_assert (!is_accessible (bases_of (^^K, uctx)[1], gctx));
static_assert (size_of (bases_of (^^K, uctx)[1]) == sizeof (I));
static_assert (offset_of (bases_of (^^K, uctx)[1]).total_bits () == __CHAR_BIT__ * sizeof (G));
static_assert (is_base (bases_of (^^K, uctx)[2]));
static_assert (parent_of (bases_of (^^K, uctx)[2]) == ^^K);
static_assert (type_of (bases_of (^^K, uctx)[2]) == ^^H);
static_assert (identifier_of (bases_of (^^K, uctx)[2]) == "H");
static_assert (!is_accessible (bases_of (^^K, uctx)[2], gctx));
static_assert (size_of (bases_of (^^K, uctx)[2]) == sizeof (H));
static_assert (offset_of (bases_of (^^K, uctx)[2]).total_bits () == __CHAR_BIT__ * (sizeof (G) + sizeof (I)));
static_assert (is_base (bases_of (^^K, uctx)[3]));
static_assert (parent_of (bases_of (^^K, uctx)[3]) == ^^K);
static_assert (type_of (bases_of (^^K, uctx)[3]) == ^^J);
static_assert (identifier_of (bases_of (^^K, uctx)[3]) == "J");
static_assert (is_accessible (bases_of (^^K, uctx)[3], gctx));
static_assert (size_of (bases_of (^^K, uctx)[3]) == sizeof (J));
static_assert (offset_of (bases_of (^^K, uctx)[3]).total_bits () == __CHAR_BIT__ * (sizeof (G) + sizeof (I) + sizeof (H)));
static_assert (!has_inaccessible_bases (^^K, uctx));
static_assert (bases_of (^^L, uctx).size () == 1);
static_assert (is_base (bases_of (^^L, uctx)[0]));
static_assert (parent_of (bases_of (^^L, uctx)[0]) == ^^L);
static_assert (type_of (bases_of (^^L, uctx)[0]) == ^^A);
static_assert (identifier_of (bases_of (^^L, uctx)[0]) == "A");
static_assert (is_accessible (bases_of (^^L, uctx)[0], gctx));
static_assert (size_of (bases_of (^^L, uctx)[0]) == sizeof (A));
static_assert (offset_of (bases_of (^^L, uctx)[0]).total_bits () == __CHAR_BIT__ * (sizeof (void *) + sizeof (int)));
static_assert (bases_of (^^M, uctx).size () == 1);
static_assert (is_base (bases_of (^^M, uctx)[0]));
static_assert (parent_of (bases_of (^^M, uctx)[0]) == ^^M);
static_assert (type_of (bases_of (^^M, uctx)[0]) == ^^A);
static_assert (identifier_of (bases_of (^^M, uctx)[0]) == "A");
static_assert (is_accessible (bases_of (^^M, uctx)[0], gctx));
static_assert (size_of (bases_of (^^M, uctx)[0]) == sizeof (A));
static_assert (offset_of (bases_of (^^M, uctx)[0]).total_bits () == __CHAR_BIT__ * (sizeof (void *) + sizeof (int)));
static_assert (bases_of (^^N, uctx).size () == 3);
static_assert (is_base (bases_of (^^N, uctx)[0]));
static_assert (parent_of (bases_of (^^N, uctx)[0]) == ^^N);
static_assert (type_of (bases_of (^^N, uctx)[0]) == ^^L);
static_assert (identifier_of (bases_of (^^N, uctx)[0]) == "L");
static_assert (is_accessible (bases_of (^^N, uctx)[0], gctx));
static_assert (size_of (bases_of (^^N, uctx)[0]) == sizeof (L));
static_assert (offset_of (bases_of (^^N, uctx)[0]).total_bits () == 0);
static_assert (is_base (bases_of (^^N, uctx)[1]));
static_assert (parent_of (bases_of (^^N, uctx)[1]) == ^^N);
static_assert (type_of (bases_of (^^N, uctx)[1]) == ^^M);
static_assert (identifier_of (bases_of (^^N, uctx)[1]) == "M");
static_assert (!is_accessible (bases_of (^^N, uctx)[1], gctx));
static_assert (size_of (bases_of (^^N, uctx)[1]) == sizeof (M));
#if (__SIZEOF_POINTER__ == 4 || __SIZEOF_POINTER__ == 8) && __SIZEOF_INT__ == 4 && __CHAR_BIT__ == 8
// For LP64, L and M bases have vptr and the int field + 32 bits of padding,
// for ILP32 they have no padding.
static_assert (offset_of (bases_of (^^N, uctx)[1]).total_bits () == __CHAR_BIT__ * 2 * sizeof (void *));
#endif
static_assert (is_base (bases_of (^^N, uctx)[2]));
static_assert (parent_of (bases_of (^^N, uctx)[2]) == ^^N);
static_assert (type_of (bases_of (^^N, uctx)[2]) == ^^A);
static_assert (identifier_of (bases_of (^^N, uctx)[2]) == "A");
static_assert (is_accessible (bases_of (^^N, uctx)[2], gctx));
static_assert (size_of (bases_of (^^N, uctx)[2]) == sizeof (A));
#if (__SIZEOF_POINTER__ == 4 || __SIZEOF_POINTER__ == 8) && __SIZEOF_INT__ == 4 && __CHAR_BIT__ == 8
// For LP64, N::n is in the of M and then comes the A base.
// For ILP32 N::n is after the M base.
static_assert (offset_of (bases_of (^^N, uctx)[2]).total_bits () == __CHAR_BIT__ * (4 * sizeof (void *) + (sizeof (void *) == 8 ? 0 : sizeof (int))));
static_assert (offset_of (^^N::n).total_bits () == __CHAR_BIT__  * (4 * sizeof (void *) - ((sizeof (void *) == 8 ? sizeof (int) : 0))));
#endif

static_assert (bases_of (^^A, gctx).size () == 0);
static_assert (bases_of (^^B, gctx).size () == 0);
static_assert (bases_of (^^C, gctx).size () == 0);
static_assert (!has_inaccessible_bases (^^C, gctx));
static_assert (bases_of (^^D, gctx).size () == 0);
static_assert (bases_of (^^E, gctx).size () == 0);
static_assert (bases_of (^^F, gctx).size () == 0);
static_assert (bases_of (^^G, gctx).size () == 0);
static_assert (bases_of (^^H, gctx).size () == 0);
static_assert (bases_of (^^I, gctx).size () == 1);
static_assert (bases_of (^^I, gctx)[0] == bases_of (^^I, uctx)[0]);
static_assert (has_inaccessible_bases (^^I, gctx));
static_assert (bases_of (^^J, gctx).size () == 1);
static_assert (bases_of (^^J, gctx)[0] == bases_of (^^J, uctx)[2]);
static_assert (has_inaccessible_bases (^^J, gctx));
static_assert (bases_of (^^K, gctx).size () == 2);
static_assert (bases_of (^^K, gctx)[0] == bases_of (^^K, uctx)[0]);
static_assert (bases_of (^^K, gctx)[1] == bases_of (^^K, uctx)[3]);
static_assert (has_inaccessible_bases (^^K, gctx));
static_assert (bases_of (^^L, gctx).size () == 1);
static_assert (bases_of (^^L, gctx)[0] == bases_of (^^L, uctx)[0]);
static_assert (!has_inaccessible_bases (^^L, gctx));
static_assert (bases_of (^^M, gctx).size () == 1);
static_assert (bases_of (^^M, gctx)[0] == bases_of (^^M, uctx)[0]);
static_assert (!has_inaccessible_bases (^^L, gctx));
static_assert (bases_of (^^N, gctx).size () == 2);
static_assert (bases_of (^^N, gctx)[0] == bases_of (^^N, uctx)[0]);
static_assert (bases_of (^^N, gctx)[1] == bases_of (^^N, uctx)[2]);
static_assert (has_inaccessible_bases (^^N, gctx));
