// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::reflect_object.

#include <meta>

using namespace std::meta;

struct S { int m; };

template <int &> void fn();
int p[2];
static_assert(template_arguments_of(^^fn<p[1]>)[0] == reflect_object(p[1]));
int pp[2][2];
static_assert(template_arguments_of(^^fn<pp[1][1]>)[0] == reflect_object(pp[1][1]));

template <const int &P>
void
fn_int_ref ()
{
  static constexpr auto R = reflect_object(P);

  static_assert(is_object(R));
  static_assert(!is_value(R));
  static_assert(!is_variable(R));
  if constexpr (is_const(R)) {
    static_assert(type_of(R) == ^^const int);
    static_assert([:R:] == 2);
  } else {
    static_assert(type_of(R) == ^^int);
  }
}

template <const int &P>
void
fn_int_subobject_ref ()
{
  static constexpr auto R = reflect_object(P);

  static_assert(is_object(R));
  static_assert(!is_value(R));
  static_assert(!is_variable(R));
  static_assert(type_of(R) == ^^const int);
  static_assert([:R:] == 3);
}

template <S P>
void
fn_cls_value ()
{
  static constexpr auto R = reflect_object(P);

  static_assert(is_object(R));
  static_assert(!is_value(R));
  static_assert(!is_variable(R));
  static_assert(type_of(R) == ^^const S);
  static_assert([:R:].m == 5);
}

template <S &P>
void
fn_cls_ref ()
{
  static constexpr auto R = reflect_object(P);

  static_assert(is_object(R));
  static_assert(!is_value(R));
  static_assert(!is_variable(R));
  static_assert(type_of(R) == ^^S);
}

void
doit ()
{
  static constexpr int k = 2;
  fn_int_ref<k>();

  static int k2;
  fn_int_ref<k2>();

  static constexpr std::pair<int, int> p {3, 4};
  fn_int_subobject_ref<p.first>();

  fn_cls_value<{5}>();

  static S s;
  fn_cls_ref<s>();
}
