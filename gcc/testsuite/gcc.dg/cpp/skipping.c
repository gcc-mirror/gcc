/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */

/* Tests expansion of macros whilst skipping false conditionals.  */

/* Source: Neil Booth, 29 Oct 2000.  */

#define F()
#define TRUE 1

#if 0
F(			/* No diagnostic: don't even try to expand it.  */
#endif

#if 0
#elif TRUE		/* Expand this, even though we were skipping.  */
#else
#error Macros not expanded in #elif
#endif
