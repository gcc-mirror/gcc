// GROUPS passed arg-matching
// arg-matching file
// Message-Id: <9307081747.AA14030@tnt>
// From: mclaugh@tnt.acsys.com (Mark A. McLaughlin)
// Subject: g++ bug
// Date: Thu, 8 Jul 93 11:47:28 MDT


#include <iostream.h>
#include <stdio.h>

// With this declaration the program will not link.
template <class Type> ostream & save(ostream & os, Type T);

   template <class Type> ostream &
save(ostream & os, Type T) {
   return os << T;
}  // save

   int
main() {
   int i = 10;
   save((ostream &)cout, i) << endl;
   short int s = 5;
   save((ostream &)cout, s) << endl;
   printf ("PASS\n");
}  // main
