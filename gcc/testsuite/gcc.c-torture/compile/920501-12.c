/* This test fails on HC11/HC12 when it is compiled without -mshort because 
   the stack arrays are too large.  Force to use 16-bit ints for it.  */
/* { dg-do assemble } */
/* { dg-xfail-if "" { m6811-*-* m6812-*-* } { "*" } { "-mshort" } } */

x(x){            return 3 + x;}
a(x){int y[994]; return 3 + x;}
b(x){int y[999]; return 2*(x + 3);}
A(x){int y[9999];return 2*(x + 3);}
B(x){int y[9949];return 3 + x;}
