/* Copyright (C) 2000  Free Software Foundation.

   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

/* { dg-do preprocess } */

#line 1 L"foo" /* { dg-error "not a string" "wide string in #line" } */
#include L"stdio.h" /* { dg-error "expects" "wide string in #include" } */
#pragma implementation L"test.h" /* { dg-error "malformed" "wide string in #pragma implementation" } */
