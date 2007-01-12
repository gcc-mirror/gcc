/* Copyright (C) 2007 Free Software Foundation, Inc.  */
/* PR preprocessor/28227 */

/* { dg-do preprocess } */
#ifdef defined
#endif
#ifndef defined
#endif

int x;
