/* Copyright (C) 2003  Free Software Foundation.

   Verify that all the binary __builtin_ math functions are
   recognized by the compiler.

   Written by Roger Sayle, 6th February 2002.  */

/* { dg-do compile } */
/* { dg-final { scan-assembler-not "__builtin_" } } */

double test1(double x, double y) { return __builtin_pow(x,y); }
double test2(double x, double y) { return __builtin_atan2(x,y); }

float test1f(float x, float y) { return __builtin_powf(x,y); }
float test2f(float x, float y) { return __builtin_atan2f(x,y); }

long double test1l(long double x, long double y)
{ return __builtin_powl(x,y); }
long double test2l(long double x, long double y)
{ return __builtin_atan2l(x,y); }

