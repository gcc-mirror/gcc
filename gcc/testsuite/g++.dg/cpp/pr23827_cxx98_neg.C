// { dg-do compile { target c++98_only } }
/* { dg-options "-ansi -pedantic-errors" }  */

double x = 0x3.1415babep0; // { dg-error "use of C..17 hexadecimal floating constant" }
