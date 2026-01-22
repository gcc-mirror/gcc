// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::extract.

#include <meta>

using namespace std::meta;

constexpr auto rnptr = reflect_constant (nullptr_t{});
static_assert (extract<nullptr_t>(rnptr) == nullptr_t{});
static_assert (extract<const nullptr_t>(rnptr) == nullptr_t{});
constexpr auto rt = reflect_constant (true);
static_assert (extract<bool>(rt));
static_assert (extract<const bool>(rt));
constexpr auto rf = reflect_constant (false);
static_assert (!extract<bool>(rf));
static_assert (!extract<const bool>(rf));
constexpr auto r1 = reflect_constant (1);
static_assert (extract<int>(r1) == 1);
static_assert (extract<const int>(r1) == 1);
constexpr auto r1u = reflect_constant (1u);
static_assert (extract<unsigned>(r1u) == 1u);
static_assert (extract<const unsigned>(r1u) == 1u);
constexpr auto rA = reflect_constant ('A');
static_assert (extract<char>(rA) == 'A');
static_assert (extract<const char>(rA) == 'A');
constexpr auto rPIf = reflect_constant (3.14f);
static_assert (extract<float>(rPIf) == float(3.14f));
static_assert (extract<const float>(rPIf) == float(3.14f));
constexpr auto rPI = reflect_constant (3.14);
static_assert (extract<double>(rPI) == double(3.14));
static_assert (extract<const double>(rPI) == double(3.14));
constexpr auto r666 = reflect_constant (666L);
static_assert (extract<long int>(r666) == 666L);
static_assert (extract<const long int>(r666) == 666L);
constexpr short int shrt = 42;
static_assert (extract<short int>(constant_of (^^shrt)) == 42);
static_assert (extract<const short int>(constant_of (^^shrt)) == 42);
constexpr signed char sc = -1;
static_assert (extract<signed char>(constant_of (^^sc)) == -1);
static_assert (extract<const signed char>(constant_of (^^sc)) == -1);
constexpr unsigned char uc = 255;
static_assert (extract<unsigned char>(constant_of (^^uc)) == 255);
static_assert (extract<const unsigned char>(constant_of (^^uc)) == 255);
constexpr int *p = nullptr;
constexpr info rp = ^^p;
static_assert (extract<int *>(rp) == nullptr);
static_assert (extract<int * const>(rp) == nullptr);
static_assert (extract<const int *>(rp) == nullptr);
static_assert (extract<const int *const>(rp) == nullptr);
constexpr const int *cp = nullptr;
constexpr info rcp = ^^cp;
static_assert (extract<const int *>(rcp) == nullptr);
static_assert (extract<const int *const>(rcp) == nullptr);
constexpr void (*pfn0)() = nullptr;
constexpr void (*pfn1)(int) = nullptr;
constexpr info rpfn0 = ^^pfn0;
constexpr info rpfn1 = ^^pfn1;
static_assert (extract<void (*const)()>(rpfn0) == nullptr);
static_assert (extract<void (*)()>(rpfn0) == nullptr);
static_assert (extract<void (*)(int)>(rpfn1) == nullptr);
static_assert (extract<void (*const)(int)>(rpfn1) == nullptr);
constexpr int arr[] = { 1, 2, 3, 4, 5 };
static_assert (extract<const int *>(^^arr) == [: reflect_constant_array (arr) :]);
static_assert (extract<const int *const>(^^arr) == [: reflect_constant_array (arr) :]);
static_assert (extract<const int *>(^^arr)[0] == 1);
static_assert (extract<const int *>(^^arr)[1] == 2);
static_assert (extract<const int *>(^^arr)[2] == 3);
static_assert (extract<const int *>(^^arr)[3] == 4);
static_assert (extract<const int *>(^^arr)[4] == 5);
static_assert (extract<int>(reflect_constant (arr[0])) == 1);
static_assert (extract<int>(reflect_constant (arr[1])) == 2);
static_assert (extract<int>(reflect_constant (arr[2])) == 3);
static_assert (extract<int>(reflect_constant (arr[3])) == 4);
static_assert (extract<int>(reflect_constant (arr[4])) == 5);
const auto lambda = []{};
static_assert (extract<void (*)()>(^^lambda) == lambda);
static_assert (extract<void (*const)()>(^^lambda) == lambda);

enum E { A = 42 };
enum struct EC { A = 42 };
static_assert (extract<E>(reflect_constant (A)) == 42);
static_assert (extract<EC>(reflect_constant (EC::A)) == EC::A);

struct S { int m; };
static_assert (extract<S>(reflect_constant (S{42})).m == 42);

constexpr int foo () { return 42; }
static_assert (extract<int>(reflect_constant (foo ())) == 42);

constexpr int cxi = 42;
static_assert (extract<const int *>(reflect_constant (&cxi)) == &cxi);

const int ci = 42;
void fn () {}
struct C {
  int k;
  void fn ();
};

template<auto E>
consteval bool
check_val (info R)
{
  return extract<decltype (E)>(R) == E;
}
static_assert (check_val<42>(reflect_constant (42)));
static_assert (check_val<EC::A>(^^EC::A));
static_assert (check_val<E::A>(^^E::A));
static_assert (check_val<42>(^^ci));
static_assert (check_val<42>(^^cxi));
static_assert (check_val<&fn>(^^fn));
static_assert (check_val<&C::k>(^^C::k));
static_assert (check_val<&C::fn>(^^C::fn));
static_assert (check_val<42>([]() {
  constexpr static int x = 42;
  return ^^x;
}()));

template<int K> struct TC {
  static constexpr int value = K;
};

TC<3> t;
static_assert (check_val<3>(static_data_members_of
			    (substitute (^^TC, {reflect_constant (3)}),
			     access_context::unchecked ())[0]));

template<typename T>
consteval bool
roundtrip (T value)
{
  return extract<T>(reflect_constant (value)) == value;
}
static_assert (roundtrip (42));
static_assert (roundtrip (EC::A));
static_assert (roundtrip (E::A));
static_assert (roundtrip (ci));
static_assert (roundtrip (cxi));
static_assert (roundtrip (fn));
static_assert (roundtrip (&C::k));
static_assert (roundtrip (&C::fn));
static_assert (roundtrip ([] {}));

struct B {
  static constexpr int k = 42;
  int m;
  int* p;

  int fn ();
  int fn2 () noexcept;
  static int fn3 ();
  static int fn4 () noexcept;
  int fn5 (this B);
  int fn6 (int) &;
  int fn7 (int) &&;
};
static_assert (extract<const int>(^^B::k) == 42);
static_assert (extract<const int &>(^^B::k) == 42);
static_assert (extract<int (B::*)()>(^^B::fn) == &B::fn);
static_assert (extract<int (B::*)() noexcept>(^^B::fn2) == &B::fn2);
static_assert (extract<int (*)()>(^^B::fn3) == &B::fn3);
static_assert (extract<int (*)() noexcept>(^^B::fn4) == &B::fn4);
static_assert (extract<int (*)(B)>(^^B::fn5) == &B::fn5);
static_assert (extract<int (B::*)(int) &>(^^B::fn6) == &B::fn6);
static_assert (extract<int (B::*)(int) &&>(^^B::fn7) == &B::fn7);
constexpr auto a = extract<int (B::*)()>(^^B::fn);
constexpr auto a2 = extract<int (B::*)()noexcept>(^^B::fn2);
constexpr auto a3 = extract<int (*)()>(^^B::fn3);
constexpr auto a4 = extract<int (*)() noexcept>(^^B::fn4);
constexpr auto a5 = extract<int (*)(B)>(^^B::fn5);
constexpr auto a6 = extract<int (B::*)(int) &>(^^B::fn6);
constexpr auto a7 = extract<int (B::*)(int) &&>(^^B::fn7);
constexpr auto a8 = extract<int B::*>(^^B::m);
constexpr auto a9 = extract<int const B::*>(^^B::m);
constexpr auto a10 = extract<int* B::*>(^^B::p);
constexpr auto a11 = extract<int* const B::*>(^^B::p);
constexpr auto a12 = extract<int const* const B::*>(^^B::p);
constexpr auto a13 = extract<int (*)()>(^^B::fn4);
constexpr auto a14 = extract<int (B::*)()>(^^B::fn2);
