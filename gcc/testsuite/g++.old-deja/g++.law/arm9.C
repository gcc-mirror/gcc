// Build don't link: 
// GROUPS passed ARM-compliance
#include        <iostream.h>
enum f1 {
     F1
};

enum f2 {
    F2
};

class A {
public:
    void set (f1 f);
};
void A::set (f1 f) { cout << "called A f1\n";}

class B : public A {
public:
    void set (f2 f);
};
void B::set (f2 f) { cout << "called B\n";} // ERROR - candidate

int main() {
    B b;
    b.set(F1); // ARM page 309: should call A.set(f1) and that what g++ does,// ERROR - .*
               // but 13.1 of ARM clearly states that it should call B::set()
               // or generate an error because overloading works only for
               // functions within the same scope (first page of chapter 13)
               // while member of derived and base classes are considered to
               // belong to different scopes.  Thus B::set() should have
               // hidden (completely) the A::set() function.
}
