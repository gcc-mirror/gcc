/* { dg-do compile } */
/* { dg-options "-std=gnu99 -Wunsuffixed-float-constants" } */

#define VAL 0.5;

double a = 1.1d;

/* With FLOAT_CONST_DECIMAL64 switched to ON these would have type
   _Decimal64.  */

double b = VAL;		/* { dg-warning "unsuffixed floating constant" } */
double c = 1.2;		/* { dg-warning "unsuffixed floating constant" } */

/* With FLOAT_CONST_DECIMAL64 switched to ON these are still binary.  */

double d = 0x5.0p1;	/* No warning for hex constant.  */
double e = 3.1i;	/* No warning for imaginary constant.  */
