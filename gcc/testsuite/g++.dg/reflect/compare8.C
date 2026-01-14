// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Reflection of a reflection.

template <int N>
void
foo ()
{
}

template <typename ... T>
void
bar ()
{
}

template <int N>
struct R {};

template <typename ... T>
struct S {};

using info = decltype (^^int);
constexpr info r1 = ^^foo;
constexpr info r2 = ^^foo <42>;
constexpr info r3 = ^^foo <43>;
constexpr info r4 = ^^R;
constexpr info r5 = ^^R <42>;
constexpr info r6 = ^^R <43>;
constexpr info r7 = ^^bar;
constexpr info r8 = ^^bar <>;
constexpr info r9 = ^^bar <int>;
constexpr info r10 = ^^bar <int, int>;
constexpr info r11 = ^^bar <int, int, long>;
constexpr info r12 = ^^S;
constexpr info r13 = ^^S <>;
constexpr info r14 = ^^S <int>;
constexpr info r15 = ^^S <int, int>;
constexpr info r16 = ^^S <int, int, long>;
static_assert (r1 == ^^foo);
static_assert (r2 == ^^foo <42>);
static_assert (r3 == ^^foo <43>);
static_assert (r4 == ^^R);
static_assert (r5 == ^^R <42>);
static_assert (r6 == ^^R <43>);
static_assert (r7 == ^^bar);
static_assert (r8 == ^^bar <>);
static_assert (r9 == ^^bar <int>);
static_assert (r10 == ^^bar <int, int>);
static_assert (r11 == ^^bar <int, int, long>);
static_assert (r12 == ^^S);
static_assert (r13 == ^^S <>);
static_assert (r14 == ^^S <int>);
static_assert (r15 == ^^S <int, int>);
static_assert (r16 == ^^S <int, int, long>);
static_assert (r1 != r2);
static_assert (r2 != r3);
static_assert (r4 != r5);
static_assert (r5 != r6);
static_assert (r7 != r8);
static_assert (r8 != r9);
static_assert (r9 != r10);
static_assert (r10 != r11);
static_assert (r12 != r13);
static_assert (r13 != r14);
static_assert (r14 != r15);
static_assert (r15 != r16);
