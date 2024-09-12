// { dg-do assemble  }
// { dg-additional-options -fpermissive }
// { dg-skip-if "requires hosted libstdc++ for iostream" { ! hostedlib } }
// GROUPS passed missed-error
// missed-error file
// From: ndc!don@csvax.cs.caltech.edu (Don Erway)
// Date:     Thu, 21 May 92 15:40:45 PDT
// Subject:  More on [g++ 2.1 : overloaded function selection incorrect]
// Message-ID: <9205212240.AA17934@ndc.com>

#include <iostream>

// The VxWorks kernel-mode headers define a macro named "max", which is not
// ISO-compliant, but is part of the VxWorks API.
#if defined __vxworks && !defined __RTP__
#undef max
#endif

inline int max(int a, int b) {return a > b ? a : b;}; // { dg-message "note" } 
 // { dg-error "extra ';'" "extra ;" { target c++98_only } .-1 }
inline double max(double a, double b) {return a > b ? a : b;}; // { dg-message "note" } candidate
 // { dg-error "extra ';'" "extra ;" { target c++98_only } .-1 }

int main() {
  // we treat this as-if extern
   static void foo(int i, int j, double x, double y) ;// { dg-error "" } .*

   foo(4, -37, 14.39, 14.38);
}

static void foo(int i, int j, double x, double y) // { dg-warning ".extern." }
{ 

   std::cout << "Max(int): " << max(i,j) << " Max(double): " <<
max(x,y) << '\n';
   std::cout << "Max(int, double): " << max(i, y) << '\n';// { dg-error "" } 
}

