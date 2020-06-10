/* This used to ICE (PR c++/29573) */
/* { dg-do compile } */

template<int> struct A {};

template<typename> struct B : A <sizeof(=)> {}; /* { dg-error "" } */

template<typename> struct C : A <sizeof(=)> {}; /* { dg-error "" } */

int a;

template<typename> struct D : A <sizeof(a=1)> {}; /* This used to ICE as well. */

template<typename> struct E : A <sizeof(a=1)> {}; /* This used to ICE as well. */

