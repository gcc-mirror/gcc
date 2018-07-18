// { dg-do compile { target c++11 } }

a(int(const &&__attribute__((b(auto;))))) // { dg-error "parameter declaration|with no type|before|expected constructor" }
