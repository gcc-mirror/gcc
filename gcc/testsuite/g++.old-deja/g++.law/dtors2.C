// { dg-do run  }
// GROUPS passed destructors
#include <stdio.h>

int destruct = 0;

class bla {

public:

        inline bla(char * jim) { ; };

        inline ~bla() { destruct++; if (destruct == 2) printf ("PASS\n");};
};

class ulk {

public:

        inline ulk() {};
        inline ~ulk() {};

        void funk(const bla & bob) { ;};
             //       ^ interestingly, the code compiles right if
             //         this & is deleted (and therefore the parameter
             //         passed as value)
};

int main() {

        ulk dumm;

        dumm.funk(bla("laberababa"));  // this compiles correctly

        dumm.funk((bla)"laberababa");  // this produces incorrect code -
                                       // the temporary instance of
                                       // the class "bla" is constructed
                                       // but never destructed...


}
