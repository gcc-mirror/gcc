// { dg-do compile { target c++11 } }
#include "pch-string-nulls.H"
static_assert (X[4] == '[' && X[5] == '!' && X[6] == ']', "error");
