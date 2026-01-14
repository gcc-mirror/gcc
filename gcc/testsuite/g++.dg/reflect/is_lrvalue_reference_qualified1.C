// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_[lr]value_reference_qualified.

#include <meta>

using namespace std::meta;

struct S {
  void fn1 ();
  void fn2 () &;
  void fn3 () &&;
  static void fn4 ();
  /* Static member functions cannot have ref-qualifiers.  */
  template<typename>
  void fn5 ();
  template<typename>
  void fn6 () &;
  template<typename>
  void fn7 () &&;
};

void foo ();
template<typename>
void bar ();
/* Non-member functions cannot have ref-qualifiers.  */

static_assert (!is_lvalue_reference_qualified (^^S::fn1));
static_assert (!is_rvalue_reference_qualified (^^S::fn1));

static_assert (is_lvalue_reference_qualified (^^S::fn2));
static_assert (!is_rvalue_reference_qualified (^^S::fn2));

static_assert (!is_lvalue_reference_qualified (^^S::fn3));
static_assert (is_rvalue_reference_qualified (^^S::fn3));

static_assert (!is_lvalue_reference_qualified (^^S::fn4));
static_assert (!is_rvalue_reference_qualified (^^S::fn4));

static_assert (!is_lvalue_reference_qualified (^^S::fn5<int>));
static_assert (!is_rvalue_reference_qualified (^^S::fn5<int>));

static_assert (is_lvalue_reference_qualified (^^S::fn6<int>));
static_assert (!is_rvalue_reference_qualified (^^S::fn6<int>));

static_assert (!is_lvalue_reference_qualified (^^S::fn7<int>));
static_assert (is_rvalue_reference_qualified (^^S::fn7<int>));

static_assert (!is_lvalue_reference_qualified (^^::foo));
static_assert (!is_rvalue_reference_qualified (^^::foo));

static_assert (!is_lvalue_reference_qualified (^^::bar<int>));
static_assert (!is_rvalue_reference_qualified (^^::bar<int>));

static_assert (!is_lvalue_reference_qualified (type_of (^^S::fn1)));
static_assert (!is_rvalue_reference_qualified (type_of (^^S::fn1)));

static_assert (is_lvalue_reference_qualified (type_of (^^S::fn2)));
static_assert (!is_rvalue_reference_qualified (type_of (^^S::fn2)));

static_assert (!is_lvalue_reference_qualified (type_of (^^S::fn3)));
static_assert (is_rvalue_reference_qualified (type_of (^^S::fn3)));

static_assert (!is_lvalue_reference_qualified (type_of (^^S::fn4)));
static_assert (!is_rvalue_reference_qualified (type_of (^^S::fn4)));

static_assert (!is_lvalue_reference_qualified (type_of (^^S::fn5<int>)));
static_assert (!is_rvalue_reference_qualified (type_of (^^S::fn5<int>)));

static_assert (is_lvalue_reference_qualified (type_of (^^S::fn6<int>)));
static_assert (!is_rvalue_reference_qualified (type_of (^^S::fn6<int>)));

static_assert (!is_lvalue_reference_qualified (type_of (^^S::fn7<int>)));
static_assert (is_rvalue_reference_qualified (type_of (^^S::fn7<int>)));

static_assert (!is_lvalue_reference_qualified (type_of (^^::foo)));
static_assert (!is_rvalue_reference_qualified (type_of (^^::foo)));

static_assert (!is_lvalue_reference_qualified (type_of (^^::bar<int>)));
static_assert (!is_rvalue_reference_qualified (type_of (^^::bar<int>)));

static_assert (!is_lvalue_reference_qualified (^^void (int)));
static_assert (!is_rvalue_reference_qualified (^^void (int)));

static_assert (is_lvalue_reference_qualified (^^void (int) &));
static_assert (!is_rvalue_reference_qualified (^^void (int) &));

static_assert (!is_lvalue_reference_qualified (^^void (int) &&));
static_assert (is_rvalue_reference_qualified (^^void (int) &&));

static_assert (!is_lvalue_reference_qualified (std::meta::info {}));
static_assert (!is_lvalue_reference_qualified (^^int));
static_assert (!is_lvalue_reference_qualified (^^::));
