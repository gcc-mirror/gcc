// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// Make sure the error about '<:' can be turned into a warning
// { dg-options "-fpermissive -fshow-column" }

struct B;

template <class A>
struct Foo {};

Foo<::B> foo;   // { dg-bogus "error" "error in place of warning" }
// { dg-error "4: warning: '<::' cannot begin a template-argument list" "warning <::" { target *-*-* } 11 }
// { dg-error "4: note: '<:' is an alternate spelling for '.'. Insert whitespace between '<' and '::'" "note <:" { target *-*-* } 11 }
