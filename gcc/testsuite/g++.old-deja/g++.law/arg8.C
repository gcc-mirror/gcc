// GROUPS passed arg-matching
// arg-matching file
// Message-Id: <9307081747.AA14030@tnt>
// From: mclaugh@tnt.acsys.com (Mark A. McLaughlin)
// Subject: g++ bug
// Date: Thu, 8 Jul 93 11:47:28 MDT


#include <iostream>
#include <cstdio>

// With this declaration the program will not link.
template <class Type> std::ostream & save(std::ostream & os, Type T);

   template <class Type> std::ostream &
save(std::ostream & os, Type T) {
   return os << T;
}  // save

   int
main() {
   int i = 10;
   save((std::ostream &)std::cout, i) << std::endl;
   short int s = 5;
   save((std::ostream &)std::cout, s) << std::endl;
   std::printf ("PASS\n");
}  // main
