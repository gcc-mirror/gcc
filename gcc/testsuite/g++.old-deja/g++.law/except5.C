// { dg-do assemble  }
// { dg-options "-fexceptions" }
// GROUPS passed exceptions
// except file
// Message-Id: <199311101607.AA11803@hsi86.hsi.com>
// From: Grigory Tsipenyuk <grigory@hsi.com>
// Subject: exception's bug?
// Date: Wed, 10 Nov 1993 11:07:12 -0500

#include <iostream>

class X {
        int     *a;
        int     sz;
public:
        class range { }; // exception class
        X(int s)        { a=new int[sz=s]; }
        int& operator[](int i);
};

int& X::operator[](int i)
{
        if (i < 0 || i >= sz) {
                throw range();
        }
        return a[i];
}

int
main()
{
        X       c(10);
        try {
                for (int i = 0; i < 12; i++)
                        c[i] = 1;
        } catch (X::range) {
                std::cerr << "invalid range\n";
        }
        return 0;
}
