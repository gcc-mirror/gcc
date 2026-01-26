// PR c++/123823
// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

constexpr int i = 42;
constexpr auto r = ^^i;
static_assert ([:r:] < 43);
static_assert ([:r:] <= 43);
static_assert ([:r:] > 41);
static_assert ([:r:] >= 41);
static_assert ([:r:] == 42);
static_assert ([:r:] != 41);

static_assert (43 > [:r:]);
static_assert (43 >= [:r:]);
static_assert (41 < [:r:]);
static_assert (41 <= [:r:]);
static_assert (42 == [:r:]);
static_assert (41 != [:r:]);

static_assert ([:r:] < 86 >> 1);
static_assert ([:r:] < 43 > 0);
static_assert (!([:r:] < 42 > 0));

template<bool>
struct S;
template<>
struct S<true> { };

S<[:r:] < 43> s1;
S<[:r:] <= 43> s2;
// [temp.names]/4 -> need the ().
S<([:r:] > 41)> s3;
S<[:r:] >= 41> s4;
S<[:r:] == 42> s5;
S<[:r:] != 41> s6;

S<(43 > [:r:])> s7;
S<43 >= [:r:]> s8;
S<41 < [:r:]> s9;
S<41 <= [:r:]> s10;
S<42 == [:r:]> s11;
S<41 != [:r:]> s12;
