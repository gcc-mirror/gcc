// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

#include <meta>

using namespace std::meta;

struct S {
  constexpr int foo (int x) const { return 41 + x + s; }
  constexpr int foo (long x) const { return 42 + x + s; }
  constexpr int foo (long long x) const { return 43 + x + s; }
  static constexpr int bar (int x) { return 72 + x; } 
  static constexpr int bar (long x) { return 73 + x; } 
  static constexpr int bar (long long x) { return 74 + x; } 
  constexpr S (int x) : s (x) {}
  int s;
};
struct T : public S {
  constexpr int foo (int x) const { return 51 + x + s; }
  constexpr int foo (long x) const { return 52 + x + s; }
  constexpr int foo (long long x) const { return 53 + x + s; }
  static constexpr int bar (int x) { return 82 + x; } 
  static constexpr int bar (long x) { return 83 + x; } 
  static constexpr int bar (long long x) { return 84 + x; } 
  constexpr T (int x) : S (x) {}
};

constexpr access_context uctx = access_context::unchecked ();

constexpr S s = 1;
static_assert (s.foo (0) == 42);
static_assert (s.foo (0L) == 43);
static_assert (s.foo (0LL) == 44);
static_assert (s.[:members_of (^^S, uctx)[0]:] (10) == 52);
static_assert (s.[:members_of (^^S, uctx)[0]:] (20L) == 62);
static_assert (s.[:members_of (^^S, uctx)[0]:] (30LL) == 72);
static_assert (s.[:members_of (^^S, uctx)[1]:] (10) == 53);
static_assert (s.[:members_of (^^S, uctx)[1]:] (20L) == 63);
static_assert (s.[:members_of (^^S, uctx)[1]:] (30LL) == 73);
static_assert (s.[:members_of (^^S, uctx)[2]:] (10) == 54);
static_assert (s.[:members_of (^^S, uctx)[2]:] (20L) == 64);
static_assert (s.[:members_of (^^S, uctx)[2]:] (30LL) == 74);
static_assert (s.bar (0) == 72);
static_assert (s.bar (0L) == 73);
static_assert (s.bar (0LL) == 74);
static_assert (s.[:members_of (^^S, uctx)[3]:] (10) == 82);
static_assert (s.[:members_of (^^S, uctx)[3]:] (20L) == 92);
static_assert (s.[:members_of (^^S, uctx)[3]:] (30LL) == 102);
static_assert (s.[:members_of (^^S, uctx)[4]:] (10) == 83);
static_assert (s.[:members_of (^^S, uctx)[4]:] (20L) == 93);
static_assert (s.[:members_of (^^S, uctx)[4]:] (30LL) == 103);
static_assert (s.[:members_of (^^S, uctx)[5]:] (10) == 84);
static_assert (s.[:members_of (^^S, uctx)[5]:] (20L) == 94);
static_assert (s.[:members_of (^^S, uctx)[5]:] (30LL) == 104);
constexpr T t = 2;
static_assert (t.foo (0) == 53);
static_assert (t.foo (0L) == 54);
static_assert (t.foo (0LL) == 55);
static_assert (t.[:members_of (^^S, uctx)[0]:] (10) == 53);
static_assert (t.[:members_of (^^S, uctx)[0]:] (20L) == 63);
static_assert (t.[:members_of (^^S, uctx)[0]:] (30LL) == 73);
static_assert (t.[:members_of (^^S, uctx)[1]:] (10) == 54);
static_assert (t.[:members_of (^^S, uctx)[1]:] (20L) == 64);
static_assert (t.[:members_of (^^S, uctx)[1]:] (30LL) == 74);
static_assert (t.[:members_of (^^S, uctx)[2]:] (10) == 55);
static_assert (t.[:members_of (^^S, uctx)[2]:] (20L) == 65);
static_assert (t.[:members_of (^^S, uctx)[2]:] (30LL) == 75);
static_assert (t.[:members_of (^^T, uctx)[0]:] (10) == 63);
static_assert (t.[:members_of (^^T, uctx)[0]:] (20L) == 73);
static_assert (t.[:members_of (^^T, uctx)[0]:] (30LL) == 83);
static_assert (t.[:members_of (^^T, uctx)[1]:] (10) == 64);
static_assert (t.[:members_of (^^T, uctx)[1]:] (20L) == 74);
static_assert (t.[:members_of (^^T, uctx)[1]:] (30LL) == 84);
static_assert (t.[:members_of (^^T, uctx)[2]:] (10) == 65);
static_assert (t.[:members_of (^^T, uctx)[2]:] (20L) == 75);
static_assert (t.[:members_of (^^T, uctx)[2]:] (30LL) == 85);
static_assert (t.bar (0) == 82);
static_assert (t.bar (0L) == 83);
static_assert (t.bar (0LL) == 84);
static_assert (t.[:members_of (^^S, uctx)[3]:] (10) == 82);
static_assert (t.[:members_of (^^S, uctx)[3]:] (20L) == 92);
static_assert (t.[:members_of (^^S, uctx)[3]:] (30LL) == 102);
static_assert (t.[:members_of (^^S, uctx)[4]:] (10) == 83);
static_assert (t.[:members_of (^^S, uctx)[4]:] (20L) == 93);
static_assert (t.[:members_of (^^S, uctx)[4]:] (30LL) == 103);
static_assert (t.[:members_of (^^S, uctx)[5]:] (10) == 84);
static_assert (t.[:members_of (^^S, uctx)[5]:] (20L) == 94);
static_assert (t.[:members_of (^^S, uctx)[5]:] (30LL) == 104);
static_assert (t.[:members_of (^^T, uctx)[3]:] (10) == 92);
static_assert (t.[:members_of (^^T, uctx)[3]:] (20L) == 102);
static_assert (t.[:members_of (^^T, uctx)[3]:] (30LL) == 112);
static_assert (t.[:members_of (^^T, uctx)[4]:] (10) == 93);
static_assert (t.[:members_of (^^T, uctx)[4]:] (20L) == 103);
static_assert (t.[:members_of (^^T, uctx)[4]:] (30LL) == 113);
static_assert (t.[:members_of (^^T, uctx)[5]:] (10) == 94);
static_assert (t.[:members_of (^^T, uctx)[5]:] (20L) == 104);
static_assert (t.[:members_of (^^T, uctx)[5]:] (30LL) == 114);

template <int N>
void
foo ()
{
  constexpr S s = 1;
  static_assert (s.foo (0) == 42);
  static_assert (s.foo (0L) == 43);
  static_assert (s.foo (0LL) == 44);
  static_assert (s.[:members_of (^^S, uctx)[0]:] (10) == 52);
  static_assert (s.[:members_of (^^S, uctx)[0]:] (20L) == 62);
  static_assert (s.[:members_of (^^S, uctx)[0]:] (30LL) == 72);
  static_assert (s.[:members_of (^^S, uctx)[1]:] (10) == 53);
  static_assert (s.[:members_of (^^S, uctx)[1]:] (20L) == 63);
  static_assert (s.[:members_of (^^S, uctx)[1]:] (30LL) == 73);
  static_assert (s.[:members_of (^^S, uctx)[2]:] (10) == 54);
  static_assert (s.[:members_of (^^S, uctx)[2]:] (20L) == 64);
  static_assert (s.[:members_of (^^S, uctx)[2]:] (30LL) == 74);
  static_assert (s.bar (0) == 72);
  static_assert (s.bar (0L) == 73);
  static_assert (s.bar (0LL) == 74);
  static_assert (s.[:members_of (^^S, uctx)[3]:] (10) == 82);
  static_assert (s.[:members_of (^^S, uctx)[3]:] (20L) == 92);
  static_assert (s.[:members_of (^^S, uctx)[3]:] (30LL) == 102);
  static_assert (s.[:members_of (^^S, uctx)[4]:] (10) == 83);
  static_assert (s.[:members_of (^^S, uctx)[4]:] (20L) == 93);
  static_assert (s.[:members_of (^^S, uctx)[4]:] (30LL) == 103);
  static_assert (s.[:members_of (^^S, uctx)[5]:] (10) == 84);
  static_assert (s.[:members_of (^^S, uctx)[5]:] (20L) == 94);
  static_assert (s.[:members_of (^^S, uctx)[5]:] (30LL) == 104);
  constexpr T t = 2;
  static_assert (t.foo (0) == 53);
  static_assert (t.foo (0L) == 54);
  static_assert (t.foo (0LL) == 55);
  static_assert (t.[:members_of (^^S, uctx)[0]:] (10) == 53);
  static_assert (t.[:members_of (^^S, uctx)[0]:] (20L) == 63);
  static_assert (t.[:members_of (^^S, uctx)[0]:] (30LL) == 73);
  static_assert (t.[:members_of (^^S, uctx)[1]:] (10) == 54);
  static_assert (t.[:members_of (^^S, uctx)[1]:] (20L) == 64);
  static_assert (t.[:members_of (^^S, uctx)[1]:] (30LL) == 74);
  static_assert (t.[:members_of (^^S, uctx)[2]:] (10) == 55);
  static_assert (t.[:members_of (^^S, uctx)[2]:] (20L) == 65);
  static_assert (t.[:members_of (^^S, uctx)[2]:] (30LL) == 75);
  static_assert (t.[:members_of (^^T, uctx)[0]:] (10) == 63);
  static_assert (t.[:members_of (^^T, uctx)[0]:] (20L) == 73);
  static_assert (t.[:members_of (^^T, uctx)[0]:] (30LL) == 83);
  static_assert (t.[:members_of (^^T, uctx)[1]:] (10) == 64);
  static_assert (t.[:members_of (^^T, uctx)[1]:] (20L) == 74);
  static_assert (t.[:members_of (^^T, uctx)[1]:] (30LL) == 84);
  static_assert (t.[:members_of (^^T, uctx)[2]:] (10) == 65);
  static_assert (t.[:members_of (^^T, uctx)[2]:] (20L) == 75);
  static_assert (t.[:members_of (^^T, uctx)[2]:] (30LL) == 85);
  static_assert (t.bar (0) == 82);
  static_assert (t.bar (0L) == 83);
  static_assert (t.bar (0LL) == 84);
  static_assert (t.[:members_of (^^S, uctx)[3]:] (10) == 82);
  static_assert (t.[:members_of (^^S, uctx)[3]:] (20L) == 92);
  static_assert (t.[:members_of (^^S, uctx)[3]:] (30LL) == 102);
  static_assert (t.[:members_of (^^S, uctx)[4]:] (10) == 83);
  static_assert (t.[:members_of (^^S, uctx)[4]:] (20L) == 93);
  static_assert (t.[:members_of (^^S, uctx)[4]:] (30LL) == 103);
  static_assert (t.[:members_of (^^S, uctx)[5]:] (10) == 84);
  static_assert (t.[:members_of (^^S, uctx)[5]:] (20L) == 94);
  static_assert (t.[:members_of (^^S, uctx)[5]:] (30LL) == 104);
  static_assert (t.[:members_of (^^T, uctx)[3]:] (10) == 92);
  static_assert (t.[:members_of (^^T, uctx)[3]:] (20L) == 102);
  static_assert (t.[:members_of (^^T, uctx)[3]:] (30LL) == 112);
  static_assert (t.[:members_of (^^T, uctx)[4]:] (10) == 93);
  static_assert (t.[:members_of (^^T, uctx)[4]:] (20L) == 103);
  static_assert (t.[:members_of (^^T, uctx)[4]:] (30LL) == 113);
  static_assert (t.[:members_of (^^T, uctx)[5]:] (10) == 94);
  static_assert (t.[:members_of (^^T, uctx)[5]:] (20L) == 104);
  static_assert (t.[:members_of (^^T, uctx)[5]:] (30LL) == 114);
}

template <typename A, typename B>
void
bar ()
{
  constexpr A s = 1;
  static_assert (s.foo (0) == 42);
  static_assert (s.foo (0L) == 43);
  static_assert (s.foo (0LL) == 44);
  static_assert (s.[:members_of (^^S, uctx)[0]:] (10) == 52);
  static_assert (s.[:members_of (^^S, uctx)[0]:] (20L) == 62);
  static_assert (s.[:members_of (^^S, uctx)[0]:] (30LL) == 72);
  static_assert (s.[:members_of (^^S, uctx)[1]:] (10) == 53);
  static_assert (s.[:members_of (^^S, uctx)[1]:] (20L) == 63);
  static_assert (s.[:members_of (^^S, uctx)[1]:] (30LL) == 73);
  static_assert (s.[:members_of (^^S, uctx)[2]:] (10) == 54);
  static_assert (s.[:members_of (^^S, uctx)[2]:] (20L) == 64);
  static_assert (s.[:members_of (^^S, uctx)[2]:] (30LL) == 74);
  static_assert (s.bar (0) == 72);
  static_assert (s.bar (0L) == 73);
  static_assert (s.bar (0LL) == 74);
  static_assert (s.[:members_of (^^S, uctx)[3]:] (10) == 82);
  static_assert (s.[:members_of (^^S, uctx)[3]:] (20L) == 92);
  static_assert (s.[:members_of (^^S, uctx)[3]:] (30LL) == 102);
  static_assert (s.[:members_of (^^S, uctx)[4]:] (10) == 83);
  static_assert (s.[:members_of (^^S, uctx)[4]:] (20L) == 93);
  static_assert (s.[:members_of (^^S, uctx)[4]:] (30LL) == 103);
  static_assert (s.[:members_of (^^S, uctx)[5]:] (10) == 84);
  static_assert (s.[:members_of (^^S, uctx)[5]:] (20L) == 94);
  static_assert (s.[:members_of (^^S, uctx)[5]:] (30LL) == 104);
  constexpr B t = 2;
  static_assert (t.foo (0) == 53);
  static_assert (t.foo (0L) == 54);
  static_assert (t.foo (0LL) == 55);
  static_assert (t.[:members_of (^^S, uctx)[0]:] (10) == 53);
  static_assert (t.[:members_of (^^S, uctx)[0]:] (20L) == 63);
  static_assert (t.[:members_of (^^S, uctx)[0]:] (30LL) == 73);
  static_assert (t.[:members_of (^^S, uctx)[1]:] (10) == 54);
  static_assert (t.[:members_of (^^S, uctx)[1]:] (20L) == 64);
  static_assert (t.[:members_of (^^S, uctx)[1]:] (30LL) == 74);
  static_assert (t.[:members_of (^^S, uctx)[2]:] (10) == 55);
  static_assert (t.[:members_of (^^S, uctx)[2]:] (20L) == 65);
  static_assert (t.[:members_of (^^S, uctx)[2]:] (30LL) == 75);
  static_assert (t.[:members_of (^^T, uctx)[0]:] (10) == 63);
  static_assert (t.[:members_of (^^T, uctx)[0]:] (20L) == 73);
  static_assert (t.[:members_of (^^T, uctx)[0]:] (30LL) == 83);
  static_assert (t.[:members_of (^^T, uctx)[1]:] (10) == 64);
  static_assert (t.[:members_of (^^T, uctx)[1]:] (20L) == 74);
  static_assert (t.[:members_of (^^T, uctx)[1]:] (30LL) == 84);
  static_assert (t.[:members_of (^^T, uctx)[2]:] (10) == 65);
  static_assert (t.[:members_of (^^T, uctx)[2]:] (20L) == 75);
  static_assert (t.[:members_of (^^T, uctx)[2]:] (30LL) == 85);
  static_assert (t.bar (0) == 82);
  static_assert (t.bar (0L) == 83);
  static_assert (t.bar (0LL) == 84);
  static_assert (t.[:members_of (^^S, uctx)[3]:] (10) == 82);
  static_assert (t.[:members_of (^^S, uctx)[3]:] (20L) == 92);
  static_assert (t.[:members_of (^^S, uctx)[3]:] (30LL) == 102);
  static_assert (t.[:members_of (^^S, uctx)[4]:] (10) == 83);
  static_assert (t.[:members_of (^^S, uctx)[4]:] (20L) == 93);
  static_assert (t.[:members_of (^^S, uctx)[4]:] (30LL) == 103);
  static_assert (t.[:members_of (^^S, uctx)[5]:] (10) == 84);
  static_assert (t.[:members_of (^^S, uctx)[5]:] (20L) == 94);
  static_assert (t.[:members_of (^^S, uctx)[5]:] (30LL) == 104);
  static_assert (t.[:members_of (^^T, uctx)[3]:] (10) == 92);
  static_assert (t.[:members_of (^^T, uctx)[3]:] (20L) == 102);
  static_assert (t.[:members_of (^^T, uctx)[3]:] (30LL) == 112);
  static_assert (t.[:members_of (^^T, uctx)[4]:] (10) == 93);
  static_assert (t.[:members_of (^^T, uctx)[4]:] (20L) == 103);
  static_assert (t.[:members_of (^^T, uctx)[4]:] (30LL) == 113);
  static_assert (t.[:members_of (^^T, uctx)[5]:] (10) == 94);
  static_assert (t.[:members_of (^^T, uctx)[5]:] (20L) == 104);
  static_assert (t.[:members_of (^^T, uctx)[5]:] (30LL) == 114);
}

template <typename A, typename B>
void
baz ()
{
  constexpr S s = 1;
  static_assert (s.foo (0) == 42);
  static_assert (s.foo (0L) == 43);
  static_assert (s.foo (0LL) == 44);
  static_assert (s.[:members_of (^^A, uctx)[0]:] (10) == 52);
  static_assert (s.[:members_of (^^A, uctx)[0]:] (20L) == 62);
  static_assert (s.[:members_of (^^A, uctx)[0]:] (30LL) == 72);
  static_assert (s.[:members_of (^^A, uctx)[1]:] (10) == 53);
  static_assert (s.[:members_of (^^A, uctx)[1]:] (20L) == 63);
  static_assert (s.[:members_of (^^A, uctx)[1]:] (30LL) == 73);
  static_assert (s.[:members_of (^^A, uctx)[2]:] (10) == 54);
  static_assert (s.[:members_of (^^A, uctx)[2]:] (20L) == 64);
  static_assert (s.[:members_of (^^A, uctx)[2]:] (30LL) == 74);
  static_assert (s.bar (0) == 72);
  static_assert (s.bar (0L) == 73);
  static_assert (s.bar (0LL) == 74);
  static_assert (s.[:members_of (^^A, uctx)[3]:] (10) == 82);
  static_assert (s.[:members_of (^^A, uctx)[3]:] (20L) == 92);
  static_assert (s.[:members_of (^^A, uctx)[3]:] (30LL) == 102);
  static_assert (s.[:members_of (^^A, uctx)[4]:] (10) == 83);
  static_assert (s.[:members_of (^^A, uctx)[4]:] (20L) == 93);
  static_assert (s.[:members_of (^^A, uctx)[4]:] (30LL) == 103);
  static_assert (s.[:members_of (^^A, uctx)[5]:] (10) == 84);
  static_assert (s.[:members_of (^^A, uctx)[5]:] (20L) == 94);
  static_assert (s.[:members_of (^^A, uctx)[5]:] (30LL) == 104);
  constexpr T t = 2;
  static_assert (t.foo (0) == 53);
  static_assert (t.foo (0L) == 54);
  static_assert (t.foo (0LL) == 55);
  static_assert (t.[:members_of (^^A, uctx)[0]:] (10) == 53);
  static_assert (t.[:members_of (^^A, uctx)[0]:] (20L) == 63);
  static_assert (t.[:members_of (^^A, uctx)[0]:] (30LL) == 73);
  static_assert (t.[:members_of (^^A, uctx)[1]:] (10) == 54);
  static_assert (t.[:members_of (^^A, uctx)[1]:] (20L) == 64);
  static_assert (t.[:members_of (^^A, uctx)[1]:] (30LL) == 74);
  static_assert (t.[:members_of (^^A, uctx)[2]:] (10) == 55);
  static_assert (t.[:members_of (^^A, uctx)[2]:] (20L) == 65);
  static_assert (t.[:members_of (^^A, uctx)[2]:] (30LL) == 75);
  static_assert (t.[:members_of (^^B, uctx)[0]:] (10) == 63);
  static_assert (t.[:members_of (^^B, uctx)[0]:] (20L) == 73);
  static_assert (t.[:members_of (^^B, uctx)[0]:] (30LL) == 83);
  static_assert (t.[:members_of (^^B, uctx)[1]:] (10) == 64);
  static_assert (t.[:members_of (^^B, uctx)[1]:] (20L) == 74);
  static_assert (t.[:members_of (^^B, uctx)[1]:] (30LL) == 84);
  static_assert (t.[:members_of (^^B, uctx)[2]:] (10) == 65);
  static_assert (t.[:members_of (^^B, uctx)[2]:] (20L) == 75);
  static_assert (t.[:members_of (^^B, uctx)[2]:] (30LL) == 85);
  static_assert (t.bar (0) == 82);
  static_assert (t.bar (0L) == 83);
  static_assert (t.bar (0LL) == 84);
  static_assert (t.[:members_of (^^A, uctx)[3]:] (10) == 82);
  static_assert (t.[:members_of (^^A, uctx)[3]:] (20L) == 92);
  static_assert (t.[:members_of (^^A, uctx)[3]:] (30LL) == 102);
  static_assert (t.[:members_of (^^A, uctx)[4]:] (10) == 83);
  static_assert (t.[:members_of (^^A, uctx)[4]:] (20L) == 93);
  static_assert (t.[:members_of (^^A, uctx)[4]:] (30LL) == 103);
  static_assert (t.[:members_of (^^A, uctx)[5]:] (10) == 84);
  static_assert (t.[:members_of (^^A, uctx)[5]:] (20L) == 94);
  static_assert (t.[:members_of (^^A, uctx)[5]:] (30LL) == 104);
  static_assert (t.[:members_of (^^B, uctx)[3]:] (10) == 92);
  static_assert (t.[:members_of (^^B, uctx)[3]:] (20L) == 102);
  static_assert (t.[:members_of (^^B, uctx)[3]:] (30LL) == 112);
  static_assert (t.[:members_of (^^B, uctx)[4]:] (10) == 93);
  static_assert (t.[:members_of (^^B, uctx)[4]:] (20L) == 103);
  static_assert (t.[:members_of (^^B, uctx)[4]:] (30LL) == 113);
  static_assert (t.[:members_of (^^B, uctx)[5]:] (10) == 94);
  static_assert (t.[:members_of (^^B, uctx)[5]:] (20L) == 104);
  static_assert (t.[:members_of (^^B, uctx)[5]:] (30LL) == 114);
}

template <typename A, typename B>
void
qux ()
{
  constexpr A s = 1;
  static_assert (s.foo (0) == 42);
  static_assert (s.foo (0L) == 43);
  static_assert (s.foo (0LL) == 44);
  static_assert (s.[:members_of (^^A, uctx)[0]:] (10) == 52);
  static_assert (s.[:members_of (^^A, uctx)[0]:] (20L) == 62);
  static_assert (s.[:members_of (^^A, uctx)[0]:] (30LL) == 72);
  static_assert (s.[:members_of (^^A, uctx)[1]:] (10) == 53);
  static_assert (s.[:members_of (^^A, uctx)[1]:] (20L) == 63);
  static_assert (s.[:members_of (^^A, uctx)[1]:] (30LL) == 73);
  static_assert (s.[:members_of (^^A, uctx)[2]:] (10) == 54);
  static_assert (s.[:members_of (^^A, uctx)[2]:] (20L) == 64);
  static_assert (s.[:members_of (^^A, uctx)[2]:] (30LL) == 74);
  static_assert (s.bar (0) == 72);
  static_assert (s.bar (0L) == 73);
  static_assert (s.bar (0LL) == 74);
  static_assert (s.[:members_of (^^A, uctx)[3]:] (10) == 82);
  static_assert (s.[:members_of (^^A, uctx)[3]:] (20L) == 92);
  static_assert (s.[:members_of (^^A, uctx)[3]:] (30LL) == 102);
  static_assert (s.[:members_of (^^A, uctx)[4]:] (10) == 83);
  static_assert (s.[:members_of (^^A, uctx)[4]:] (20L) == 93);
  static_assert (s.[:members_of (^^A, uctx)[4]:] (30LL) == 103);
  static_assert (s.[:members_of (^^A, uctx)[5]:] (10) == 84);
  static_assert (s.[:members_of (^^A, uctx)[5]:] (20L) == 94);
  static_assert (s.[:members_of (^^A, uctx)[5]:] (30LL) == 104);
  constexpr B t = 2;
  static_assert (t.foo (0) == 53);
  static_assert (t.foo (0L) == 54);
  static_assert (t.foo (0LL) == 55);
  static_assert (t.[:members_of (^^A, uctx)[0]:] (10) == 53);
  static_assert (t.[:members_of (^^A, uctx)[0]:] (20L) == 63);
  static_assert (t.[:members_of (^^A, uctx)[0]:] (30LL) == 73);
  static_assert (t.[:members_of (^^A, uctx)[1]:] (10) == 54);
  static_assert (t.[:members_of (^^A, uctx)[1]:] (20L) == 64);
  static_assert (t.[:members_of (^^A, uctx)[1]:] (30LL) == 74);
  static_assert (t.[:members_of (^^A, uctx)[2]:] (10) == 55);
  static_assert (t.[:members_of (^^A, uctx)[2]:] (20L) == 65);
  static_assert (t.[:members_of (^^A, uctx)[2]:] (30LL) == 75);
  static_assert (t.[:members_of (^^B, uctx)[0]:] (10) == 63);
  static_assert (t.[:members_of (^^B, uctx)[0]:] (20L) == 73);
  static_assert (t.[:members_of (^^B, uctx)[0]:] (30LL) == 83);
  static_assert (t.[:members_of (^^B, uctx)[1]:] (10) == 64);
  static_assert (t.[:members_of (^^B, uctx)[1]:] (20L) == 74);
  static_assert (t.[:members_of (^^B, uctx)[1]:] (30LL) == 84);
  static_assert (t.[:members_of (^^B, uctx)[2]:] (10) == 65);
  static_assert (t.[:members_of (^^B, uctx)[2]:] (20L) == 75);
  static_assert (t.[:members_of (^^B, uctx)[2]:] (30LL) == 85);
  static_assert (t.bar (0) == 82);
  static_assert (t.bar (0L) == 83);
  static_assert (t.bar (0LL) == 84);
  static_assert (t.[:members_of (^^A, uctx)[3]:] (10) == 82);
  static_assert (t.[:members_of (^^A, uctx)[3]:] (20L) == 92);
  static_assert (t.[:members_of (^^A, uctx)[3]:] (30LL) == 102);
  static_assert (t.[:members_of (^^A, uctx)[4]:] (10) == 83);
  static_assert (t.[:members_of (^^A, uctx)[4]:] (20L) == 93);
  static_assert (t.[:members_of (^^A, uctx)[4]:] (30LL) == 103);
  static_assert (t.[:members_of (^^A, uctx)[5]:] (10) == 84);
  static_assert (t.[:members_of (^^A, uctx)[5]:] (20L) == 94);
  static_assert (t.[:members_of (^^A, uctx)[5]:] (30LL) == 104);
  static_assert (t.[:members_of (^^B, uctx)[3]:] (10) == 92);
  static_assert (t.[:members_of (^^B, uctx)[3]:] (20L) == 102);
  static_assert (t.[:members_of (^^B, uctx)[3]:] (30LL) == 112);
  static_assert (t.[:members_of (^^B, uctx)[4]:] (10) == 93);
  static_assert (t.[:members_of (^^B, uctx)[4]:] (20L) == 103);
  static_assert (t.[:members_of (^^B, uctx)[4]:] (30LL) == 113);
  static_assert (t.[:members_of (^^B, uctx)[5]:] (10) == 94);
  static_assert (t.[:members_of (^^B, uctx)[5]:] (20L) == 104);
  static_assert (t.[:members_of (^^B, uctx)[5]:] (30LL) == 114);
}

void
fred ()
{
  foo <42> ();
  bar <S, T> ();
  baz <S, T> ();
  qux <S, T> ();
}
