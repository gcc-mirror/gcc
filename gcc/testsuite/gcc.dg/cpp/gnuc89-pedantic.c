/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-std=gnu89 -pedantic" } */

/* This file is for testing the preprocessor in -std=gnu89 -pedantic mode.
   Neil Booth, 2 Dec 2000.  */

#if 1LL				/* { dg-warning "too many" } */
#endif
