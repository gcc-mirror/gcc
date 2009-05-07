// { dg-do compile }
// { dg-options "-Wunknown-pragmas" }

#pragma STDC FLOAT_CONST_DECIMAL64 ON	// { dg-warning "not supported for C\\\+\\\+" }
double d = 1.0;
