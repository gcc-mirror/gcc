// { dg-do compile }
// Origin: Giovanni Bajo <giovannibajo at gcc dot gnu dot org>
// Make sure the error about '<:' can be turned into a warning
// { dg-options "-fpermissive -fshow-column" }

struct B;

template <class A>
struct Foo {};

Foo<::B> foo;   // { dg-bogus "error" "error in place of warning" { target { ! c++11 } } }
// { dg-warning "4: '<::' cannot begin a template-argument list" "warning <::" { target { ! c++11 } } .-1 }
// { dg-message "4:'<:' is an alternate spelling for '.'. Insert whitespace between '<' and '::'" "note <:" { target { ! c++11 } } .-2 }
