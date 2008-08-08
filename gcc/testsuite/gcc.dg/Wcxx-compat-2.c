/* { dg-options "-Wc++-compat" } */

_Bool foo;			/* This is okay.  */
int bool;			/* { dg-warning "keyword" } */
int catch;			/* { dg-warning "keyword" } */
int char16_t;			/* { dg-warning "keyword" } */
int char32_t;			/* { dg-warning "keyword" } */
int class;			/* { dg-warning "keyword" } */
int const_cast;			/* { dg-warning "keyword" } */
int decltype;			/* { dg-warning "keyword" } */
int delete;			/* { dg-warning "keyword" } */
int dynamic_cast;		/* { dg-warning "keyword" } */
int explicit;			/* { dg-warning "keyword" } */
int export;			/* { dg-warning "keyword" } */
int false;			/* { dg-warning "keyword" } */
int friend;			/* { dg-warning "keyword" } */
int mutable;			/* { dg-warning "keyword" } */
int namespace;			/* { dg-warning "keyword" } */
int new;			/* { dg-warning "keyword" } */
int operator;			/* { dg-warning "keyword" } */
int private;			/* { dg-warning "keyword" } */
int protected;			/* { dg-warning "keyword" } */
int public;			/* { dg-warning "keyword" } */
int reinterpret_cast;		/* { dg-warning "keyword" } */
int static_assert;		/* { dg-warning "keyword" } */
int static_cast;		/* { dg-warning "keyword" } */
int template;			/* { dg-warning "keyword" } */
int this;			/* { dg-warning "keyword" } */
int throw;			/* { dg-warning "keyword" } */
int true;			/* { dg-warning "keyword" } */
int try;			/* { dg-warning "keyword" } */
int typename;			/* { dg-warning "keyword" } */
int typeid;			/* { dg-warning "keyword" } */
int using;			/* { dg-warning "keyword" } */
int virtual;			/* { dg-warning "keyword" } */
int wchar_t;
