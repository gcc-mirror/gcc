// PR c++/5658

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Theodore.Papadopoulo 11 Feb 2002 <Theodore.Papadopoulo@sophia.inria.fr>

struct A {
    typedef int iterator;
};
template <typename T>
struct B: public A {
    template <typename U>
    struct iterator {
    };
    B() { }
};
int main() 
{
    B<int> a; 
};
