// { dg-do assemble  }
// GROUPS passed bad-errors
#include <iostream>

class ParX
        {
        public:
                ParX() {}
        };

class X : public ParX
        {
        public:
                void fn2() { std::cout << "hi" << std::endl; }
        };

int main()
        {
        X               x;
        ParX*   pParX                   = &x;
        void    (ParX::*p)()    = (void (ParX::*)()) &X::fn2;    // line 19

        (pParX->*p)();
        }






