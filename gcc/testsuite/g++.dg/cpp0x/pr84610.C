// { dg-do compile { target c++11 } }

a (int &__attribute__ ((aligned(auto x)) y)); // { dg-error "parameter declaration|before|expected constructor" }
