// PR c++/102482
// { dg-do compile { target c++11 } }
// Test we don't warn for non-const lvalue refs.

#include <initializer_list>

struct X { };

struct span
{
  span(std::initializer_list<int>& il)
  : begin(il.begin()) // { dg-bogus "initializer_list" }
  { }

  const int* begin;
};

struct span_warn
{
  span_warn(std::initializer_list<int> il)
  : begin(il.begin()) // { dg-warning "initializer_list" }
  { }

  const int* begin;
};

struct span_warn2
{
  span_warn2(std::initializer_list<int>&& il)
  : begin(il.begin()) // { dg-warning "initializer_list" }
  { }

  const int* begin;
};

struct span_warn3
{
  span_warn3(std::initializer_list<int> const& il)
  : begin(il.begin()) // { dg-warning "initializer_list" }
  { }

  const int* begin;
};

struct span_warn4
{
  span_warn4(std::initializer_list<int> const il)
  : begin(il.begin()) // { dg-warning "initializer_list" }
  { }

  const int* begin;
};

struct span_warn5
{
  span_warn5(std::initializer_list<int> const&& il)
  : begin(il.begin()) // { dg-warning "initializer_list" }
  { }

  const int* begin;
};
