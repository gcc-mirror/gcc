/* Copyright (C) 2000, 2001  Free Software Foundation.

   by Alexandre Oliva  <oliva@lsd.ic.unicamp.br>  */

/* { dg-do preprocess } */

#line 1 L"foo" /* { dg-error "not a valid filename" "wide string in #line" } */
#include L"stdio.h" /* { dg-error "expects" "wide string in #include" } */

