// Build don't link: 
// GROUPS passed constructors
// ctor file
// Message-Id: <199306151813.gD28471@mail.Germany.EU.net>
// From: stephan@ifconnection.de (Stephan Muehlstrasser)
// Subject: gcc 2.4.3.1: illegal constructor call not rejected
// Date: Tue, 15 Jun 1993 18:34:14 +0200 (MET DST)


#include <fstream.h>

class X : public ifstream {
    public:
      X(int a, char *b) {} // ERROR - candidate
}; // ERROR - candidate
int main()
{
    X *y = new X(10, "123");
    // the compiler must reject this constructor call:
    X *x = new X("abc");// ERROR - .*
}
