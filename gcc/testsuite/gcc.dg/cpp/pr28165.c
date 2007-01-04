/* Copyright (C) 2007 Free Software Foundation, Inc.  */
/* PR preprocessor/28165 */

/* { dg-do preprocess } */
#pragma GCC system_header   /* { dg-warning "system_header" "ignored" } */
_Pragma ("GCC system_header")   /* { dg-warning "system_header" "ignored" } */
