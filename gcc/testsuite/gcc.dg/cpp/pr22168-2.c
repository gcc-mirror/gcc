/* Copyright (C) 2008 Free Software Foundation, Inc.  */
/* PR preprocessor/22168 */

/* { dg-do preprocess }
   { dg-options -Wdeprecated } */
#if #foo(bar)    /* { dg-warning "deprecated" } */
int x;
#else
int y;
#endif
#assert zzz(a)   /* { dg-warning "deprecated" } */
#unassert yyy    /* { dg-warning "deprecated" } */
