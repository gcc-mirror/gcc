/* DR 1511 - const volatile variables and the one-definition rule */
/* { dg-do compile } */

typedef const int cint;
typedef const volatile int cvint;
typedef volatile int vint;
const int v1 = 5;
volatile const int v2 = 6;
cint v3 = 7;
cvint v4 = 8;
const vint v5 = 9;
volatile cint v6 = 10;
const int w1 = 5;
volatile const int w2 = 6;
cint w3 = 7;
cvint w4 = 8;
const vint w5 = 9;
volatile cint w6 = 10;
const int &r1 = w1;
volatile const int &r2 = w2;
const int &r3 = w3;
const volatile int &r4 = w4;
const volatile int &r5 = w5;
const volatile int &r6 = w6;
