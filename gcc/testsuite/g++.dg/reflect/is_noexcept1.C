// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_noexcept.

#include <meta>

// Basic noexcept specifications
void noexcept_function () noexcept;
void noexcept_true_function () noexcept(true);
void noexcept_false_function () noexcept(false);
void not_noexcept_function ();

static_assert(std::meta::is_noexcept (^^noexcept_function));
static_assert(std::meta::is_noexcept (^^noexcept_true_function));
static_assert(!std::meta::is_noexcept (^^noexcept_false_function));
static_assert(!std::meta::is_noexcept (^^not_noexcept_function));

// throw()
void throw_function () throw();
// void func () throw(int) -->  error: ISO C++17 does not allow dynamic exception specifications

static_assert (std::meta::is_noexcept (^^throw_function));

// Template functions
template <class T>
void noexcept_template_function(T param) noexcept(true);

// Note 2 from P2996: If r represents a function template that is declared noexcept,
// is_noexcept(r) is still false because in general such queries
// for templates cannot be answered.
static_assert (!std::meta::is_noexcept (^^noexcept_template_function));
// Note 5 from P2996: A template specialization is not a template.
static_assert (std::meta::is_noexcept (^^noexcept_template_function<bool>));

template <class T>
void template_function(T param);

static_assert (!std::meta::is_noexcept (^^template_function));
static_assert (!std::meta::is_noexcept (^^template_function<int>));

template<bool B>
void conditional_noexcept() noexcept(B);

static_assert (!std::meta::is_noexcept (^^conditional_noexcept));
static_assert (std::meta::is_noexcept (^^conditional_noexcept<true>));
static_assert (!std::meta::is_noexcept (^^conditional_noexcept<false>));

struct S {
  void noexcept_method() noexcept;
  void noexcept_true_method() noexcept(true);
  void noexcept_false_method() noexcept(false);
  void not_noexcept_method();

  virtual void noexcept_virtual_method() noexcept {}
  virtual void noexcept_true_virtual_method() noexcept(true) {}
  virtual void noexcept_false_virtual_method() noexcept(false) {}
  virtual void not_noexcept_virtual_method() {}

  template <typename T>
  void noexcept_template_method() noexcept;
  template <typename T>
  void noexcept_true_template_method() noexcept(true);
  template <typename T>
  void noexcept_false_template_method() noexcept(false);
  template <typename T>
  void noexcept_cond_template_method() noexcept(sizeof(T) > 4);
  template <typename T>
  void not_noexcept_template_method();
};

// Methods
static_assert (std::meta::is_noexcept (^^S::noexcept_method));
static_assert (std::meta::is_noexcept (^^S::noexcept_true_method));
static_assert (!std::meta::is_noexcept (^^S::noexcept_false_method));
static_assert (!std::meta::is_noexcept (^^S::not_noexcept_method));

// Virtual methods
static_assert (std::meta::is_noexcept (^^S::noexcept_virtual_method));
static_assert (std::meta::is_noexcept (^^S::noexcept_true_virtual_method));
static_assert (!std::meta::is_noexcept (^^S::noexcept_false_virtual_method));
static_assert (!std::meta::is_noexcept (^^S::not_noexcept_virtual_method));

// Template methods
static_assert (!std::meta::is_noexcept (^^S::noexcept_template_method));
static_assert (!std::meta::is_noexcept (^^S::noexcept_true_template_method));
static_assert (!std::meta::is_noexcept (^^S::noexcept_false_template_method));
static_assert (!std::meta::is_noexcept (^^S::noexcept_cond_template_method));
static_assert (!std::meta::is_noexcept (^^S::not_noexcept_template_method));

static_assert (std::meta::is_noexcept (^^S::noexcept_template_method<int>));
static_assert (std::meta::is_noexcept (^^S::noexcept_true_template_method<int>));
static_assert (!std::meta::is_noexcept (^^S::noexcept_false_template_method<int>));
static_assert (!std::meta::is_noexcept (^^S::not_noexcept_template_method<int>));

struct SS : public S {

};

// Inherited methods
static_assert (std::meta::is_noexcept (^^SS::noexcept_method));
static_assert (std::meta::is_noexcept (^^SS::noexcept_true_method));
static_assert (!std::meta::is_noexcept (^^SS::noexcept_false_method));
static_assert (!std::meta::is_noexcept (^^SS::not_noexcept_method));

// Function types
static_assert (std::meta::is_noexcept (^^void() noexcept));
static_assert (!std::meta::is_noexcept (^^void()));

using not_noexcept_func_type = void();
using noexcept_func_type = void() noexcept;

static_assert (!std::meta::is_noexcept (^^not_noexcept_func_type));
static_assert (std::meta::is_noexcept (^^noexcept_func_type));

template<typename T, bool B>
using conditional_noexcept_func_type = void(T) noexcept(B);

static_assert (!std::meta::is_noexcept (^^conditional_noexcept_func_type));
static_assert (!std::meta::is_noexcept (^^conditional_noexcept_func_type<int, false>));
static_assert (std::meta::is_noexcept (^^conditional_noexcept_func_type<int, true>));

// Constructors
struct T {
    T() = default;

    T(int) {
      int a;
      static_assert (!std::meta::is_noexcept (std::meta::parent_of (^^a)));
    }

    T(double) noexcept {
      int a;
      static_assert (std::meta::is_noexcept (std::meta::parent_of (^^a)));
    }

    T(bool) noexcept(true) {
      int a;
      static_assert (std::meta::is_noexcept (std::meta::parent_of (^^a)));
    }

    T(S) noexcept(false) {
      int a;
      static_assert (!std::meta::is_noexcept (std::meta::parent_of (^^a)));
    }
};

static_assert (noexcept (T()));
static_assert (!noexcept (T(123)));
static_assert (noexcept (T(123.123)));
static_assert (noexcept (T(true)));
static_assert (!noexcept (T(S())));

// Destructors
struct U { };
U u;
static_assert (std::meta::is_noexcept (^^U::~U));
static_assert (noexcept (u.~U()));

struct V { ~V() = delete; };
static_assert (std::meta::is_noexcept (^^V::~V));

struct W { ~W() = default; };
W w;
static_assert (std::meta::is_noexcept (^^W::~W));
static_assert (noexcept (w.~W()));

struct X { ~X(); };
X x;
static_assert (std::meta::is_noexcept (^^X::~X));
static_assert (noexcept (x.~X()));

struct Y { ~Y() noexcept(true); };
Y y;
static_assert (std::meta::is_noexcept (^^Y::~Y));
static_assert (noexcept (y.~Y()));

struct Z { ~Z() noexcept(false); };
Z z;
static_assert(!std::meta::is_noexcept (^^Z::~Z));
static_assert(!noexcept (z.~Z()));
