/* { dg-do assemble } */
/* { dg-skip-if "Array too big" { "pdp11-*-*" } { "-mint32" } } */
/* { dg-require-stack-size "9999*4" } */

int x(int x){            return 3 + x;}
int a(int x){int y[994]; return 3 + x;}
int b(int x){int y[999]; return 2*(x + 3);}
int A(int x){int y[9999];return 2*(x + 3);}
int B(int x){int y[9949];return 3 + x;}
