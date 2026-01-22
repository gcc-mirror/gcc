// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::extract.

#include <meta>

using namespace std::meta;

template<typename T>
consteval bool
can_extract (info r)
{
  try { extract<T>(r); }
  catch (std::meta::exception &) { return false; }
  return true;
}

constexpr info null_reflection;
static_assert (!can_extract<int>(null_reflection));

constexpr auto rt = reflect_constant (true);
static_assert (!can_extract<int>(rt));
static_assert (!can_extract<char>(rt));
static_assert (!can_extract<short>(rt));
static_assert (!can_extract<long>(rt));
static_assert (!can_extract<float>(rt));
static_assert (!can_extract<bool *>(rt));
constexpr auto rA = reflect_constant ('A');
static_assert (!can_extract<bool>(rA));
static_assert (!can_extract<int>(rA));
static_assert (!can_extract<unsigned short>(rA));
static_assert (!can_extract<long>(rA));
static_assert (!can_extract<double>(rA));
static_assert (!can_extract<int *>(rA));
constexpr short int shrt = 42;
constexpr info rshrt = constant_of (^^shrt);
static_assert (!can_extract<int *>(rshrt));
static_assert (!can_extract<bool>(rshrt));
static_assert (!can_extract<signed char>(rshrt));
static_assert (!can_extract<int>(rshrt));
static_assert (!can_extract<double>(rshrt));
static_assert (!can_extract<long>(rshrt));
constexpr signed char sc = -1;
constexpr info rsc = constant_of (^^sc);
static_assert (!can_extract<int *>(rsc));
static_assert (!can_extract<bool>(rsc));
static_assert (!can_extract<unsigned char>(rsc));
static_assert (!can_extract<int>(rsc));
static_assert (!can_extract<double>(rsc));
static_assert (!can_extract<long>(rsc));
constexpr unsigned char uc = 255;
constexpr info ruc = constant_of (^^uc);
static_assert (!can_extract<int *>(ruc));
static_assert (!can_extract<bool>(ruc));
static_assert (!can_extract<signed char>(ruc));
static_assert (!can_extract<int>(ruc));
static_assert (!can_extract<double>(ruc));
static_assert (!can_extract<long>(ruc));

constexpr const int *p = nullptr;
constexpr auto rp = ^^p;
static_assert (!can_extract<int *>(rp));
static_assert (!can_extract<char *>(rp));

constexpr void (*pfn0)() = nullptr;
constexpr void (*pfn1)(int) = nullptr;
constexpr info rpfn0 = ^^pfn0;
constexpr info rpfn1 = ^^pfn1;
static_assert (!can_extract<void (*)(int)>(rpfn0));
static_assert (!can_extract<void (*)()>(rpfn1));

constexpr int arr[] = { 1, 2, 3, 4, 5 };
static_assert (!can_extract<int *>(^^arr));
static_assert (!can_extract<const double *>(^^arr));

const auto lambda = []{};
static_assert (!can_extract<void (*)(int)>(^^lambda));

enum E { X = 42 };
enum struct EC { Y = 42 };
static_assert (!can_extract<int *>(reflect_constant (X)));
static_assert (!can_extract<int>(reflect_constant (EC::Y)));

struct S { int m; };
static_assert (!can_extract<int>(reflect_constant (S{42})));

constexpr int foo () { return 42; }
static_assert (!can_extract<int *>(reflect_constant (foo ())));

static constexpr int i = 0;
static_assert (!can_extract<const int>(reflect_constant (&i)));
static_assert (!can_extract<const int &>(reflect_constant (i)));
static_assert (!can_extract<int &>(^^i));

int j;
static_assert (extract<int &>(^^j)); // { dg-error "non-constant|not usable" }

consteval info
bar ()
{
   constexpr int x = 10;
   return ^^x;
};

constexpr info rbar = bar ();
static_assert (!can_extract<int &>(rbar));

struct B {
  int k;
  int bf : 16;
};

static_assert (!can_extract<int>(^^B::bf));
static_assert (!can_extract<int &>(^^B::bf));
static_assert (!can_extract<const int &>(^^B::bf));
static_assert (!can_extract<int B::*>(^^B::bf));

static union { int um; };
static_assert (!can_extract<int>(^^um));
static_assert (!can_extract<int&>(^^um));

struct C {
  int fn ();
  int fn2 () noexcept;
  static int fn3 ();
  static int fn4 () noexcept;
  int fn5 (this B);
  int fn6 (int) &;
  int fn7 (int) &&;
};
static_assert (!can_extract<void (C::*)()>(^^C::fn));
static_assert (!can_extract<int (C::*)() noexcept>(^^C::fn));
static_assert (can_extract<int (C::*)()>(^^C::fn2));
static_assert (!can_extract<int (C::*)()>(^^C::fn3));
static_assert (!can_extract<int (C::*)()>(^^C::fn4));
static_assert (!can_extract<int (*)() noexcept>(^^C::fn3));
static_assert (can_extract<int (*)()>(^^C::fn4));
static_assert (!can_extract<int (*)()>(^^C::fn5));
static_assert (!can_extract<int (C::*)(int) &&>(^^C::fn6));
static_assert (!can_extract<int (C::*)(int) &>(^^C::fn7));
