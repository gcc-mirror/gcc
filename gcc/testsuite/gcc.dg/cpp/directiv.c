/* Copyright (C) 2000 Free Software Foundation, Inc.  */

/* { dg-do preprocess } */
/* { dg-options -pedantic } */

/* Tests general directive syntax, and directive error recovery.  */


/* Test directive name is not expanded.  */
#define foo define
#foo EMPTY			/* { dg-error "invalid" } */

/* Test # must be first on line.  */
EMPTY #define bar
#ifdef bar
#error bar is defined
#endif

/* Test form feed and vertical tab warn pedantically, see 6.10
   paragraph 5.  Tab is OK.  */
#define something		/* { dg-warning "form feed" } */
#define something_else	/* { dg-warning "vertical tab" } */
#define some	thing		/* Tab OK, as is form feed before #.  */

/* Our friend the null directive OK?  */
#

/* Check that directives always start a line, even if in middle of
   macro expansion.  */
#define func(x) x
func (2				/* { dg-error "unterminated invocation" }  */
#define foobar			/* { dg-error "may not be used inside" } */

/* For tidiness, I think the directive should still be processed
   above.  Certainly, continuing to try to find the closing ')' can
   lead to some really confusing error messages.  Hence this test.  */
#ifndef foobar
#error It is nice if the directive is processed!
#endif

/* Check newlines end directives, even in function-like macro
   invocations.  6.10 paragraph 1.

   Note that the #if is still treated as a conditional, so there
   should be no errors about #endif without #if.  */
#if func (			/* { dg-error "unterminated invocation" } */
#endif
