// Build don't link: 
#include <exception>
//using namespace std;
class A {
    class B : public std::exception {}
    ;
};
