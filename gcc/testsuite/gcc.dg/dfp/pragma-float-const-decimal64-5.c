/* { dg-do compile } */
/* { dg-options "-std=c99 -pedantic" } */

/* N1312 7.1.1: The FLOAT_CONST_DECIMAL64 pragma.
   C99 6.4.4.2a (New).

   Check that there is a pedantic warning for the use of pragma
   STD FLOAT_CONST_DECIMAL64.  */

double a;

void f1 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 ON		/* { dg-warning "ISO C" } */
  a = 1.0;
}

void f2 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 OFF		/* { dg-warning "ISO C" } */
  a = 2.0;
}

void f3 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 DEFAULT	/* { dg-warning "ISO C" } */
  a = 3.0;
}

void f4 (void)
{
  _Pragma ("STDC FLOAT_CONST_DECIMAL64 ON")	/* { dg-warning "ISO C" } */
  a = 1.0;
}

void f5 (void)
{
  _Pragma ("STDC FLOAT_CONST_DECIMAL64 OFF")	/* { dg-warning "ISO C" } */
  a = 2.0;
}

void f6 (void)
{
  _Pragma ("STDC FLOAT_CONST_DECIMAL64 DEFAULT")	/* { dg-warning "ISO C" } */
  a = 3.0;
}
