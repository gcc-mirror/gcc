/* Test for warnings about nontraditional directives inside the unused
   clauses of #if statements.  Extensions do _not_ receive pedantic
   warnings inside unused clauses because they are often hidden this
   way on purpose.  However they do still require indentation for K&R.  */
/* { dg-do preprocess } */
/* { dg-options "-pedantic -Wtraditional -fno-show-column" } */

#if 1

/* Block 1: K+R directives should have the # indented to warn.  */

#define foo bar		/* { dg-bogus "indented" "^#kandr"     } */
# define foo bar	/* { dg-bogus "indented" "^# kandr"    } */
 #define foo bar	/* { dg-warning "indented" "^ #kandr"  } */
 # define foo bar	/* { dg-warning "indented" "^ # kandr" } */

/* Block 2: C89 directives should not have the # indented to warn.  */

#pragma whatever	/* { dg-warning "indented" "^#c89"     } */
# pragma whatever	/* { dg-warning "indented" "^# c89"    } */
 #pragma whatever	/* { dg-bogus "indented" "^ #c89"      } */
 # pragma whatever	/* { dg-bogus "indented" "^ # c89"     } */

/* Block 3: Extensions should not have the # indented to warn, _and_
   they should get a -pedantic warning. */

#assert foo(bar)	/* { dg-warning "indented" "^#ext"    } */
# assert bar(baz)	/* { dg-warning "indented" "^# ext"   } */
 #assert baz(quux)	/* { dg-bogus "indented" "^ #ext"     } */
 # assert quux(weeble)	/* { dg-bogus "indented" "^ # ext"    } */

#else

/* Block 1: K+R directives should have the # indented to warn.  */

#undef foo bar		/* { dg-bogus "indented" "^#kandr"     } */
# undef foo bar		/* { dg-bogus "indented" "^# kandr"    } */
 #undef foo bar		/* { dg-warning "indented" "^ #kandr"  } */
 # undef foo bar	/* { dg-warning "indented" "^ # kandr" } */

/* Block 2: C89 directives should not have the # indented to warn.  */

#error whatever		/* { dg-warning "indented" "^#c89"     } */
# error whatever	/* { dg-warning "indented" "^# c89"    } */
 #error whatever	/* { dg-bogus "indented" "^ #c89"      } */
 # error whatever	/* { dg-bogus "indented" "^ # c89"     } */

/* Block 3: Extensions should not have the # indented to warn, and
   they should _not_ get a -pedantic warning. */

#unassert foo(bar)		/* { dg-warning "indented" "^#ext"    } */
# unassert bar(baz)		/* { dg-warning "indented" "^# ext"   } */
 #unassert baz(quux)		/* { dg-bogus "indented" "^ #ext"     } */
 # unassert quux(weeble)	/* { dg-bogus "indented" "^ # ext"    } */

#endif

/* { dg-warning "GCC extension" "extension warning" { target *-*-* } 27 } */
/* { dg-warning "GCC extension" "extension warning" { target *-*-* } 28 } */
/* { dg-warning "GCC extension" "extension warning" { target *-*-* } 29 } */
/* { dg-warning "GCC extension" "extension warning" { target *-*-* } 30 } */
