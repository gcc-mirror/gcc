// Copyright (C) 2002 Free Software Foundation
// Origin: C++/729
// Contributed by Gabriel Dos Reis <gdr@integrable-solutions.net>
// { dg-do compile }

struct A {
   A(int) { }
};

struct B {
   typedef B T;
   B(A, char**) {}
};

int main(int argc, char** argv)
{
   B::T t(A(argc), argv);
}
