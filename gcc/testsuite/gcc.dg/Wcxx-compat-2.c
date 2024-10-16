/* { dg-options "-std=gnu17 -Wc++-compat" } */

_Bool foo;			/* This is okay.  */
int bool;			/* { dg-warning "5:keyword" } */
int catch;			/* { dg-warning "5:keyword" } */
int char16_t;			/* { dg-warning "5:keyword" } */
int char32_t;			/* { dg-warning "5:keyword" } */
int class;			/* { dg-warning "5:keyword" } */
int const_cast;			/* { dg-warning "5:keyword" } */
int constexpr;			/* { dg-warning "5:keyword" } */
int decltype;			/* { dg-warning "5:keyword" } */
int delete;			/* { dg-warning "5:keyword" } */
int dynamic_cast;		/* { dg-warning "5:keyword" } */
int explicit;			/* { dg-warning "5:keyword" } */
int export;			/* { dg-warning "5:keyword" } */
int false;			/* { dg-warning "5:keyword" } */
int friend;			/* { dg-warning "5:keyword" } */
int mutable;			/* { dg-warning "5:keyword" } */
int namespace;			/* { dg-warning "5:keyword" } */
int new;			/* { dg-warning "5:keyword" } */
int nullptr;			/* { dg-warning "5:keyword" } */
int operator;			/* { dg-warning "5:keyword" } */
int private;			/* { dg-warning "5:keyword" } */
int protected;			/* { dg-warning "5:keyword" } */
int public;			/* { dg-warning "5:keyword" } */
int reinterpret_cast;		/* { dg-warning "5:keyword" } */
int static_assert;		/* { dg-warning "5:keyword" } */
int static_cast;		/* { dg-warning "5:keyword" } */
int template;			/* { dg-warning "5:keyword" } */
int this;			/* { dg-warning "5:keyword" } */
int throw;			/* { dg-warning "5:keyword" } */
int true;			/* { dg-warning "5:keyword" } */
int try;			/* { dg-warning "5:keyword" } */
int typename;			/* { dg-warning "5:keyword" } */
int typeid;			/* { dg-warning "5:keyword" } */
int using;			/* { dg-warning "5:keyword" } */
int virtual;			/* { dg-warning "5:keyword" } */
int wchar_t;
