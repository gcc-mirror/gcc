/* Test for warnings about nontraditional directives.  */
/* { dg-do preprocess } */
/* { dg-options "-pedantic -Wtraditional" } */

/* Block 1: K+R directives should have the # at the left margin.  */

#define foo bar		/* { dg-bogus "left margin" "^#kandr"     } */
# define foo bar	/* { dg-bogus "left margin" "^# kandr"    } */
 #define foo bar	/* { dg-warning "left margin" "^ #kandr"  } */
 # define foo bar	/* { dg-warning "left margin" "^ # kandr" } */

/* Block 2: C89 directives should not have the # at the left margin.  */

#pragma whatever	/* { dg-warning "left margin" "^#c89"     } */
# pragma whatever	/* { dg-warning "left margin" "^# c89"    } */
 #pragma whatever	/* { dg-bogus "left margin" "^ #c89"      } */
 # pragma whatever	/* { dg-bogus "left margin" "^ # c89"     } */

/* Block 3: Extensions should not have the # at the left margin,
   _and_ they should get a -pedantic warning. */

#assert foo(bar)	/* { dg-warning "left margin" "^#ext"    } */
# assert bar(baz)	/* { dg-warning "left margin" "^# ext"   } */
 #assert baz(quux)	/* { dg-bogus "left margin" "^ #ext"     } */
 # assert quux(weeble)	/* { dg-bogus "left margin" "^ # ext"    } */

/* { dg-warning "ISO C does not" "extension warning" { target native } 22 } */
/* { dg-warning "ISO C does not" "extension warning" { target native } 23 } */
/* { dg-warning "ISO C does not" "extension warning" { target native } 24 } */
/* { dg-warning "ISO C does not" "extension warning" { target native } 25 } */
