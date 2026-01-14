// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflections on id-expression.

int arr[2] = { 1, 2 };

template<typename T>
void foo (T) { }

struct S {
  int i;
  operator int() { return this->i; }
  ~S();
};

static int operator+(S a, S b) { return a.i + b.i; }

void operator ""_km(long double);
template <char...> double operator ""_fm();

void
f (S a, S b)
{
  // unqualified-id
  constexpr auto r1 = ^^foo<int>;
  [: r1 :](1);

  constexpr auto r2 = ^^operator+;
  [: r2 :](a, b);

  constexpr auto r3 = ^^operator ""_km;
  constexpr auto r4 = ^^operator ""_fm;
  constexpr auto r5 = ^^operator ""_fm<'a'>;

  auto & [ c, d ] = arr;
  constexpr auto r6 = ^^c;
  constexpr auto r7 = ^^d;
  [: r6 :] = 1;
  [: r7 :] = 1;
}

void
g (S a, S b)
{
  // qualified-id
  constexpr auto r1 = ^^::foo<int>;
  [: r1 :](1);

  constexpr auto r2 = ^^::operator+;
  [: r2 :](a, b);

  constexpr auto r3 = ^^S::operator int;
  constexpr auto r4 = ^^::operator ""_km;
  constexpr auto r5 = ^^S::~S;
  constexpr auto r6 = ^^::operator ""_fm;
  constexpr auto r7 = ^^::operator ""_fm<'a'>;
}

consteval int
h ()
{
  int x = 41;
  constexpr auto rx = ^^x;
  ++[: rx :];
  return x;
}

static_assert(h () == 42);
