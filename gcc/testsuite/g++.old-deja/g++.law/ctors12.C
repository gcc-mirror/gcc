// { dg-do run  }
// GROUPS passed constructors
#include <cstdio>
#include <cstdlib>
#include <iostream>

#define MAGIC 7654

class complex {
        double re;
        double im;
        int magic;
        static int count;
public:
        complex() { re=im=0; magic=MAGIC; }
        complex(double d) { re=d; im=0; magic=MAGIC; }
        complex(double d, double d2) {re=d; im=d2; magic=MAGIC; }
        ~complex() {if(magic!=MAGIC) {std::printf("FAIL\n"); std::exit(1);}}
        friend std::ostream& operator << (std::ostream& o, const complex& c)
        { return o << "(" << c.re << "," << c.im << ")"; }
};

int complex::count=0;

int main()
{
        complex v[6] = {1, complex(1,2), complex(), 2 }; // ARM Sect. 12.6.1
        int i;                                           // page 289

        for(i=0; i<6; i++) ;
	std::printf ("PASS\n");

        return 0;
}
