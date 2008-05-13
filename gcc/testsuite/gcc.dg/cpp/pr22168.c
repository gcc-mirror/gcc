/* Copyright (C) 2008 Free Software Foundation, Inc.  */
/* PR preprocessor/22168 */

/* { dg-do preprocess }
   { dg-options -pedantic } */
#if #foo(bar)    /* { dg-warning "GCC extension" } */
int x;
#else
int y;
#endif
#assert zzz(a)   /* { dg-warning "GCC extension" } */
#unassert yyy    /* { dg-warning "GCC extension" } */
