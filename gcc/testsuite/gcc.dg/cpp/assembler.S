/* Regression test - in assembly language, # may have some significance
   other than 'stringize macro argument' and therefore must be preserved
   in the output, and should not be warned about.  */

/* { dg-do preprocess } */
/* { dg-options "" } */

#define foo() mov r0, #5  /* { dg-bogus "not followed" "spurious warning" } */

entry:
	foo()

/* Check we don't EOF on an unknown directive.  */
#unknown directive
#error a later diagnostic	/* { dg-error "diagnostic" } */
		
/*
   { dg-final { if ![file exists 20000510-1.i] { return }	} }
   { dg-final { set tmp [grep 20000510-1.i # line]		} }
   { dg-final { if {[string length $tmp] > 0} \{		} }
   { dg-final {     pass "20000510-1.S: # preservation"		} }
   { dg-final { \} else \{					} }
   { dg-final {     fail "20000510-1.S: # preservation"		} }
   { dg-final { \}						} }
*/

