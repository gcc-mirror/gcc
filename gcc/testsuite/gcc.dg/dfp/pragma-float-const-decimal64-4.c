/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

/* N1312 7.1.1: The FLOAT_CONST_DECIMAL64 pragma.
   C99 6.4.4.2a (New).

   Check that malformed versions of pragma STDC FLOAT_CONST_DECIMAL64
   are detected.  */

double a;

void f1 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64	/* { dg-warning "malformed" } */
  a = 1.0;
}

void f2 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 DFP	/* { dg-warning "malformed" } */
  a = 2.0;
}

void f3 (void)
{
#pragma STDC FLOAT_CONST_DECIMAL64 ON DFP /* { dg-warning "junk at end" } */
  a = 3.0;
}

void f4 (void)
{
  _Pragma ( "STDC FLOAT_CONST_DECIMAL64" )	/* { dg-warning "malformed" } */
  a = 1.0;
}

void f5 (void)
{
  _Pragma ( "STDC FLOAT_CONST_DECIMAL64 DFP" )	/* { dg-warning "malformed" } */
  a = 2.0;
}

void f6 (void)
{
  _Pragma ( "STDC FLOAT_CONST_DECIMAL64 ON DFP" ) /* { dg-warning "junk at end" } */
  a = 3.0;
}
