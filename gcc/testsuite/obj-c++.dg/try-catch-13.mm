/* Ensure that variables declared volatile by the user (as opposed to
   synthesized by the EH-volatization machinery) _do_ trigger 
   "discards qualifiers from target pointer type" warnings.  */

/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

@interface TestMyTests
- (void) testSpoon;
@end

extern void some_func (int *); /* { dg-line some_func_decl } */

@implementation TestMyTests
- (void) testSpoon {
  volatile int i = 5;
  int q = 99;

  do {
    @try {
      typeof(i) j = 6;
      typeof(q) k = 66;
      some_func (&j);
/* { dg-error "invalid conversion" "" { target *-*-* } .-1 } */ 
/* { dg-message "initializing argument" "" { target *-*-* } some_func_decl } */
      some_func (&k);
    }
    @catch (id exc) {
      @throw;
    }
  } while(0);

  do {
    @try {
      typeof(i) j = 7;
      typeof(q) k = 77;
      some_func (&k);
      some_func (&j);
/* { dg-error "invalid conversion" "" { target *-*-* } .-1 } */
/* The following is disabled as it is already checked above and the testsuites seems 
   to count multiple different identical errors on the same line only once */
/*  dg-message "initializing argument" "" { target *-*-* } some_func_decl  */
    }
    @catch (id exc) {
      @throw;
    }
  } while(0);

  do {
    @try {
      typeof(q) k = 88;
      typeof(i) j = 8;
      some_func (&j); 
/* { dg-error "invalid conversion" "" { target *-*-* } .-1 } */
/* The following is disabled as it is already checked above and the testsuites seems 
   to count multiple different identical errors on the same line only once */
/*  dg-message "initializing argument" "" { target *-*-* } some_func_decl  */
      some_func (&k);
    }
    @catch (id exc) {
      @throw;
    }
  } while(0);
      
}
@end

