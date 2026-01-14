// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test reflection type traits [meta.reflection.traits], const-volatile
// modifications.

#include <meta>
using namespace std::meta;

struct C { };

static_assert (remove_const (^^const int) == ^^int);
static_assert (remove_const (^^volatile int) == ^^volatile int);
static_assert (remove_const (^^const volatile int) == ^^volatile int);
static_assert (remove_const (^^const volatile int *) == ^^const volatile int *);
static_assert (remove_const (^^const volatile int &) == ^^const volatile int &);
static_assert (remove_const (^^const volatile int []) == ^^volatile int []);
static_assert (remove_const (^^volatile int *const) == ^^volatile int *);
static_assert (remove_const (^^const volatile C) == ^^volatile C);
static_assert (remove_const (^^void (C::* const volatile)(int) const) == ^^void (C::* volatile)(int) const);
static_assert (remove_const (^^int (int)) == ^^int (int));
using T1 = int;
static_assert (remove_const (^^T1) == dealias (^^T1));

static_assert (remove_volatile (^^const int) == ^^const int);
static_assert (remove_volatile (^^volatile int) == ^^int);
static_assert (remove_volatile (^^const volatile int) == ^^const int);
static_assert (remove_volatile (^^const volatile int *) == ^^const volatile int *);
static_assert (remove_volatile (^^const volatile int &) == ^^const volatile int &);
static_assert (remove_volatile (^^const volatile int []) == ^^const int []);
static_assert (remove_volatile (^^volatile int *const) == ^^volatile int *const);
static_assert (remove_volatile (^^volatile int *volatile) == ^^volatile int *);
static_assert (remove_volatile (^^const volatile C) == ^^const C);
static_assert (remove_volatile (^^void (C::* const volatile)(int) const) == ^^void (C::* const)(int) const);
static_assert (remove_volatile (^^int (int)) == ^^int (int));
static_assert (remove_volatile (^^T1) == dealias (^^T1));

static_assert (remove_cv (^^const int) == ^^int);
static_assert (remove_cv (^^volatile int) == ^^int);
static_assert (remove_cv (^^const volatile int) == ^^int);
static_assert (remove_cv (^^const volatile int *) == ^^const volatile int *);
static_assert (remove_cv (^^const volatile int &) == ^^const volatile int &);
static_assert (remove_cv (^^const volatile int []) == ^^int []);
static_assert (remove_cv (^^volatile int *const) == ^^volatile int *);
static_assert (remove_cv (^^const volatile C) == ^^C);
static_assert (remove_cv (^^void (C::* const volatile)(int) const) == ^^void (C::*)(int) const);
static_assert (remove_cv (^^int (int)) == ^^int (int));
static_assert (remove_cv (^^T1) == dealias (^^T1));

static_assert (add_const (^^int) == ^^const int);
static_assert (add_const (^^const int) == ^^const int);
static_assert (add_const (^^volatile int) == ^^const volatile int);
static_assert (add_const (^^const volatile int) == ^^const volatile int);
static_assert (add_const (^^const volatile int *) == ^^const volatile int *const);
static_assert (add_const (^^int []) == ^^const int[]);
static_assert (add_const (^^int *) == ^^int *const);
static_assert (add_const (^^C) == ^^const C);
static_assert (add_const (^^void (C::*)(int) const) == ^^void (C::* const)(int) const);
static_assert (add_const (^^int (int)) == ^^int (int));
static_assert (add_const (^^int &) == ^^int &);
static_assert (add_const (^^const int &&) == ^^const int &&);
using T2 = const int;
static_assert (add_const (^^T2) == dealias (^^T2));

static_assert (add_volatile (^^int) == ^^volatile int);
static_assert (add_volatile (^^const int) == ^^const volatile int);
static_assert (add_volatile (^^volatile int) == ^^volatile int);
static_assert (add_volatile (^^const volatile int) == ^^const volatile int);
static_assert (add_volatile (^^const volatile int *) == ^^const volatile int *volatile);
static_assert (add_volatile (^^int []) == ^^volatile int[]);
static_assert (add_volatile (^^int *) == ^^int *volatile);
static_assert (add_volatile (^^C) == ^^volatile C);
static_assert (add_volatile (^^void (C::*)(int) const) == ^^void (C::* volatile)(int) const);
static_assert (add_volatile (^^int (int)) == ^^int (int));
static_assert (add_volatile (^^int &) == ^^int &);
static_assert (add_volatile (^^const int &&) == ^^const int &&);
using T3 = volatile int;
static_assert (add_volatile (^^T3) == dealias (^^T3));

static_assert (add_cv (^^int) == ^^const volatile int);
static_assert (add_cv (^^const int) == ^^const volatile int);
static_assert (add_cv (^^volatile int) == ^^const volatile int);
static_assert (add_cv (^^const volatile int) == ^^const volatile int);
static_assert (add_cv (^^const volatile int *) == ^^const volatile int *const volatile);
static_assert (add_cv (^^int []) == ^^const volatile int[]);
static_assert (add_cv (^^int *) == ^^int *const volatile);
static_assert (add_cv (^^C) == ^^const volatile C);
static_assert (add_cv (^^void (C::*)(int) const) == ^^void (C::* const volatile)(int) const);
static_assert (add_cv (^^int (int)) == ^^int (int));
static_assert (add_cv (^^int &) == ^^int &);
static_assert (add_cv (^^const int &&) == ^^const int &&);
using T4 = const volatile int;
static_assert (add_cv (^^T4) == dealias (^^T4));
