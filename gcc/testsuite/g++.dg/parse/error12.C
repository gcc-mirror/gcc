// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// Make sure the error about '<:' can be turned into a warning
// { dg-options "-fpermissive" }

struct B;

template <class A>
struct Foo {};

Foo<::B> foo;   // { dg-bogus "error" "error in place of warning" }
// { dg-error "" "" { target *-*-* } 11 }
