// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::common_reference.

#include <meta>
using namespace std::meta;

template <reflection_range R = std::initializer_list <info>>
consteval bool
has_common_reference (R &&args)
{
  try { common_reference (args); }
  catch (std::meta::exception &) { return false; }
  return true;
}

struct A { };
struct B { };
struct C { };
struct D { };
struct E { };
struct F { };

template<template<typename> class AQual, template<typename> class BQual>
struct std::basic_common_reference<A, B, AQual, BQual>
{
  using type = BQual<AQual<C>>;
};

template<> struct std::common_type<D, E> { using type = F; };

static_assert (common_reference ({ ^^int }) == ^^int);
static_assert (common_reference ({ ^^int & }) == ^^int &);
static_assert (common_reference ({ ^^void }) == ^^void);
static_assert (common_reference ({ ^^const void }) == ^^const void);
static_assert (common_reference ({ ^^const void, ^^void }) == ^^void);
static_assert (common_reference ({ ^^void (*const) (), ^^void (*) () }) == ^^void (*) ());
static_assert (common_reference ({ ^^int, ^^int }) == ^^int);
static_assert (common_reference ({ ^^int &, ^^int }) == ^^int);
static_assert (common_reference ({ ^^int, ^^int & }) == ^^int);
static_assert (common_reference ({ ^^int &&, ^^int }) == ^^int);
static_assert (common_reference ({ ^^int &, ^^int & }) == ^^int &);
static_assert (common_reference ({ ^^int &, ^^int && }) == ^^const int &);
static_assert (common_reference ({ ^^int &&, ^^int & }) == ^^const int &);
static_assert (common_reference ({ ^^int &&, ^^int && }) == ^^int &&);
static_assert (common_reference ({ ^^int &&, ^^const int && }) == ^^const int &&);
static_assert (common_reference ({ ^^int &, ^^int &, ^^int && }) == ^^const int &);
static_assert (common_reference ({ ^^int &&, ^^int &, ^^int & }) == ^^const int &);
static_assert (common_reference ({ ^^char &, ^^int & }) == ^^int);
static_assert (common_reference ({ ^^long &, ^^int & }) == ^^long);
static_assert (common_reference ({ ^^A, ^^B }) == ^^C);
static_assert (common_reference ({ ^^A &, ^^B }) == ^^C &);
static_assert (common_reference ({ ^^A &, ^^const B }) == ^^C &);
static_assert (common_reference ({ ^^const A, ^^B & }) == ^^const C &);
static_assert (common_reference ({ ^^const A &, ^^B && }) == ^^const C &);
static_assert (common_reference ({ ^^const A, ^^B && }) == ^^const C &&);
static_assert (common_reference ({ ^^D, ^^E }) == ^^F);
static_assert (common_reference ({ ^^D &, ^^E }) == ^^F);
static_assert (common_reference ({ ^^D &, ^^E && }) == ^^F);
static_assert (!has_common_reference ({ ^^::, ^^int }));
static_assert (!has_common_reference ({ ^^int, ^^int, ^^std }));
