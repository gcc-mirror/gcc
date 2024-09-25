/* Test silencing C++23 #warning and P2071 escape pedwarns.  */
#pragma GCC diagnostic push

void f() {

#warning foo			/* { dg-warning foo } */
/* { dg-error "extension" "" { target { ! c++23 } } .-1 } */

'\x{0f}';		/* { dg-error "delimited escape" "" { target { ! c++23 } } }*/
"\N{OHM SIGN}";	/* { dg-error "named universal character" "" { target { ! c++23 } } }*/

#pragma GCC diagnostic ignored "-Wc++23-extensions"

#warning foo			/* { dg-warning foo } */

'\x{0f}';
"\N{OHM SIGN}";

}
