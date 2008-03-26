// { dg-options "-std=c++0x" }

struct int_placeholder;

template<typename T>
struct do_replace
{
  typedef T type;
};

template<>
struct do_replace<int_placeholder>
{
  typedef int type;
};

template<typename T> struct lambdalike
{
  typedef T type;
};

template<template<typename...> class TT, typename... Args>
struct lambdalike<TT<Args...> > {
  typedef TT<typename do_replace<Args>::type...> type;
};


template<typename T, typename U>
struct is_same
{
  static const bool value = false;
};

template<typename T>
struct is_same<T, T>
{
  static const bool value = true;
};

template<typename... Elements> struct tuple;
template<typename T1, typename T2> struct pair;

static_assert(is_same<lambdalike<tuple<float, int_placeholder, double>>::type,
		      tuple<float, int, double>>::value,
	      "MPL lambda-like replacement on tuple");
static_assert(is_same<lambdalike<pair<float, int_placeholder>>::type,
		      pair<float, int>>::value,
	      "MPL lambda-like replacement on pair");


struct _1 {};

template<typename Arg0, typename Lambda>
struct eval
{
    typedef Lambda type;
};

template<typename Arg0>
struct eval<Arg0, _1>
{
    typedef Arg0 type;
};

template<typename Arg0, template<typename...> class T, typename... Pack>
struct eval<Arg0, T<Pack...> >
{
    typedef T< typename eval<Arg0, Pack>::type... > type;
};

static_assert(is_same<eval<int, tuple<float, _1, double>>::type,
	              tuple<float, int, double>>::value, "eval tuple");
static_assert(is_same<eval<int, pair<_1, double>>::type,
	              pair<int, double>>::value, "eval pair");
static_assert(is_same<eval<int, 
	                   tuple<pair<_1, _1>, pair<float, float>,
 	                         pair<double, _1>>>::type,
	      tuple<pair<int, int>, pair<float, float>, pair<double, int>>>::value,
	      "recursive eval");
