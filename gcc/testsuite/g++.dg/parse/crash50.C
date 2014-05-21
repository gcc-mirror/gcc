// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/38636
// { dg-do compile }

struct A; // { dg-message "forward declaration of 'struct A'" }

A::A(

struct B; // { dg-error "expected '\\)' before ';' token|invalid use of incomplete type 'struct A'" }

