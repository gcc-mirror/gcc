// { dg-do run  }
// GROUPS passed destructors
#include <stdio.h>

int destruct = 2;

class a {
        public:
                char    *p;
                a(){ ; }
                ~a(){ destruct--; if (! destruct) printf ("PASS\n");}
};

a       test(){
        return a();
}

int main(){
        a       ai;

        ai = test();
}

