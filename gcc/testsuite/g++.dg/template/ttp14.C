// { dg-do compile }

// Origin: akim@epita.fr
//	   Volker Reichelt <reichelt@gcc.gnu.org>

// PR c++/18276: Template substitution of template template parameter

template<template<int> class> struct A;

template<int> struct B
{
    template<template<int> class> friend class A;
};

B<0> b;
