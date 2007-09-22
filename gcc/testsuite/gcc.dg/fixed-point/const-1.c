/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1169 6.4.4 - Constants.

   Check if the constant 1 is ok.  */

short _Fract sf = 1.0hr;
_Fract f = 1.0r;
long _Fract lf = 1.0lr;
long long _Fract llf = 1.0llr;

unsigned short _Fract usf = 1.0uhr;
unsigned _Fract uf = 1.0ur;
unsigned long _Fract ulf = 1.0ulr;
unsigned long long _Fract ullf = 1.0ullr;

short _Fract sfF = 1.0;  /* { dg-warning "overflow" } */
_Fract fF = 1.0;  /* { dg-warning "overflow" } */
long _Fract lfF = 1.0;  /* { dg-warning "overflow" } */
long long _Fract llfF = 1.0;  /* { dg-warning "overflow" } */

unsigned short _Fract usfF = 1.0;  /* { dg-warning "overflow" } */
unsigned _Fract ufF = 1.0;  /* { dg-warning "overflow" } */
unsigned long _Fract ulfF = 1.0;  /* { dg-warning "overflow" } */
unsigned long long _Fract ullfF = 1.0;  /* { dg-warning "overflow" } */

short _Fract sfI = 1;  /* { dg-warning "overflow" } */
_Fract fI = 1;  /* { dg-warning "overflow" } */
long _Fract lfI = 1;  /* { dg-warning "overflow" } */
long long _Fract llfI = 1;  /* { dg-warning "overflow" } */

unsigned short _Fract usfI = 1;  /* { dg-warning "overflow" } */
unsigned _Fract ufI = 1;  /* { dg-warning "overflow" } */
unsigned long _Fract ulfI = 1;  /* { dg-warning "overflow" } */
unsigned long long _Fract ullfI = 1;  /* { dg-warning "overflow" } */
