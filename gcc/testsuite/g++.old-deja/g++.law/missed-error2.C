// Build don't link: 
// GROUPS passed missed-error
// missed-error file
// From: ndc!don@csvax.cs.caltech.edu (Don Erway)
// Date:     Thu, 21 May 92 15:40:45 PDT
// Subject:  More on [g++ 2.1 : overloaded function selection incorrect]
// Message-ID: <9205212240.AA17934@ndc.com>

#include <iostream.h>

inline int max(int a, int b) {return a > b ? a : b;}; // ERROR - candidate
inline double max(double a, double b) {return a > b ? a : b;}; // ERROR - candidate

int main() {
   static void foo(int i, int j, double x, double y) ;// ERROR - .*

   foo(4, -37, 14.39, 14.38);
}

// 971006 we no longer give an error for this since we emit a hard error
// about the declaration above
static void foo(int i, int j, double x, double y) { 

   cout << "Max(int): " << max(i,j) << " Max(double): " <<
max(x,y) << '\n';
   cout << "Max(int, double): " << max(i, y) << '\n';// ERROR - 
}

