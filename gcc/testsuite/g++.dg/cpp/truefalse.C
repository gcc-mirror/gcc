/* Copyright (C) 2003 Free Software Foundation, Inc.  */

/* Source: Neil Booth, 18 Apr 2003.  */

/* { dg-do preprocess } */
/* { dg-options "-ansi -pedantic -Wundef" } */

/* Check that for C++ we handle true and false correctly, and do not
   treat them as undefined identifiers.  */

#if true		/* { dg-bogus "is not defined" } */
#error foo		/* { dg-error "foo" } */
#endif

#if false		/* { dg-bogus "is not defined" } */
#error foo		/* { dg-bogus "foo" } */
#endif
