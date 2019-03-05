// { dg-do compile { target c++2a } }

struct A
{
  int i;
  constexpr A (int i): i(i) {}
  // auto operator<=> (const A&) = default;
};

template <A a>
struct B
{
  static constexpr int i = a.i;
  static constexpr A const* ap = &a;
};

template <A a>
struct C
{
  static constexpr A const* ap = &a;
};

static_assert(B<1>::i == 1);
static_assert(B<2>::i == 2);
static_assert(B<1>::ap == C<1>::ap);
static_assert(B<1>::ap != C<2>::ap);

// { dg-final { scan-assembler "_Z1fP1BIXtl1ALi1EEEE" } }
// { dg-final { scan-assembler "_ZTAXtl1ALi1EEE" } }
const void* f(B<1> *p) {
  constexpr int i = p->ap->i;
  return p->ap;
}
