// { dg-do assemble  }
#include <exception>
//using namespace std;
class A {
    class B : public std::exception {}
    ;
};
