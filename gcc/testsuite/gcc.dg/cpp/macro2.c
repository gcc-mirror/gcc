/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests the argument context get set at the lower level if we drop
   contexts whilst parsing arguments.  This would enter an infinite
   loop in is_macro_disabled previously.  */

#define A Something
#define B C
#define C K(
#define K(S) S  
#define T B A )
T				/* Expands to <Something>.  */
