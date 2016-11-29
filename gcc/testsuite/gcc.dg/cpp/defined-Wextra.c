/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options "-Wextra" } */

/* Use of defined in different contexts.  */

/*  Source: Neil Booth, 29 Oct 2000, Zack Weinberg 11 Dec 2000.  */

#define Z

#define bad0 defined Z
#if !bad0                       /* { dg-warning "may not be portable" } */
#error Z is defined
#endif

#define bad1 defined
#if !bad1 Z			/* { dg-warning "may not be portable" } */
#error Z is defined
#endif 

#if !bad1 (Z)			/* { dg-warning "may not be portable" } */
#error Z is defined
#endif 

#define bad2 defined (Z
#if !bad2)			/* { dg-warning "may not be portable" } */
#error Z is defined
#endif 

