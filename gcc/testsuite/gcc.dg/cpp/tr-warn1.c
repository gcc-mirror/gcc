/* Test for warnings about nontraditional directives.  */
/* { dg-do preprocess } */
/* { dg-options "-pedantic -Wtraditional -fno-show-column" } */

/* Block 1: K+R directives should have the # indented.  */

#define foo bar		/* { dg-bogus "indented" "^#kandr"     } */
# define foo bar	/* { dg-bogus "indented" "^# kandr"    } */
 #define foo bar	/* { dg-warning "indented" "^ #kandr"  } */
 # define foo bar	/* { dg-warning "indented" "^ # kandr" } */

/* Block 2: C89 directives should not have the # indented.  */

#pragma whatever	/* { dg-warning "indented" "^#c89"     } */
# pragma whatever	/* { dg-warning "indented" "^# c89"    } */
 #pragma whatever	/* { dg-bogus "indented" "^ #c89"      } */
 # pragma whatever	/* { dg-bogus "indented" "^ # c89"     } */

/* Block 3: Extensions should not have the # indented,
   _and_ they should get a -pedantic warning. */

#assert foo(bar)	/* { dg-warning "indented" "^#ext"    } */
# assert bar(baz)	/* { dg-warning "indented" "^# ext"   } */
 #assert baz(quux)	/* { dg-bogus "indented" "^ #ext"     } */
 # assert quux(weeble)	/* { dg-bogus "indented" "^ # ext"    } */

/* We warn of #elif regardless of whether we're skipping or not, and
   do not warn about indentaion.  */
#if 0
#if 1
#elif 1			/* { dg-warning "#elif" "#elif skipping" }  */
#endif
#elif 0			/* { dg-warning "#elif" "#elif not skipping" }  */
#endif

/* { dg-warning "GCC extension" "extension warning" { target *-*-* } 22 } */
/* { dg-warning "GCC extension" "extension warning" { target *-*-* } 23 } */
/* { dg-warning "GCC extension" "extension warning" { target *-*-* } 24 } */
/* { dg-warning "GCC extension" "extension warning" { target *-*-* } 25 } */
