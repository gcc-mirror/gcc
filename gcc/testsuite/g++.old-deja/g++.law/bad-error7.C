// Build don't link: 
// GROUPS passed bad-errors
#include <iostream.h>

class ParX
        {
        public:
                ParX() {}
        };

class X : public ParX
        {
        public:
                void fn2() { cout << "hi" << endl; }
        };

int main()
        {
        X               x;
        ParX*   pParX                   = &x;
        void    (ParX::*p)()    = (void (ParX::*)()) &X::fn2;    // line 19

        (pParX->*p)();
        }
