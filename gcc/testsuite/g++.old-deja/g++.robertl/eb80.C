// Build don't link: 
#include <exception>

class A {
    class B : public std::exception {}
    ;
};
