/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-std=c++98 -pedantic" } */

/* This file is for testing the preprocessor in -std=c++98 -pedantic mode.
   Neil Booth, 2 Dec 2000.  */

#if 1LL				/* { dg-warning "long long" } */
#endif
