/* Copyright (C) 2008 Free Software Foundation, Inc.  */

/* Another test of operator precedence.  */

/* { dg-do preprocess } */
/* { dg-options "" } */

#if 1 ? 2 : 3 , 0
#error
#endif
