// { dg-do assemble  }
#include <exception>

class A {
    class B : public std::exception {}
    ;
};
