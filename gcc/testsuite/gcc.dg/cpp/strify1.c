/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Test operator # semantics.  */

#define OK1 #		/* No problem.  */
#define OK2(x) x#x	/* No problem.  */
#define bad1(x) #	/* { dg-error "followed by a macro parameter" "#1" } */
#define bad2(x) #y	/* { dg-error "followed by a macro parameter" "#2" } */
