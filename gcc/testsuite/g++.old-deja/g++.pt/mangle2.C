// { dg-do assemble  }
// 
// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 25 April 2001 <nathan@codesourcery.com>
// Origin:pcarlini@unitus.it

// Bug 2559. We hadn't implemented code to mangle numbers bigger than
// HOST_WIDE_INT.

template<class T, T min_val, T max_val>
class integer_traits_base
{
public:
static const bool is_integral = true;
};

template<class T>
class integer_traits
{
public:
static const bool is_integral = false;
};

template<>
class integer_traits<long long>
: public integer_traits_base<long long, (-9223372036854775807LL - 1),
9223372036854775807LL>
{ };

integer_traits<long long> f;

template <class T, T value> T foo () 
{
  return value;
}

void x ()
{
  foo<long long, -9223372036854775807LL> ();
}
