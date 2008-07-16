// Contributed by Dodji Seketeli <dseketel@redhat.com>
// Origin: PR c++/13699
// { dg-do compile }

namespace A {
    extern "C" void foo_func () throw(); // { dg-error "conflicts" }
}
// next line should trigger an error because
// it conflicts with previous declaration of foo_func (), due to
// different exception specifications.
extern "C" void foo_func (); // { dg-error "C language|exception specifications" }
