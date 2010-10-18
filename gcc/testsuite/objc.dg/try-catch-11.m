/* Ensure that typeof()-typed variables inside the @try { } block that
   "inherit" their EH-volatileness from other variables in the stack frame
   do not trigger "discards qualifiers from target pointer type" warnings.  */

/* { dg-options "-fobjc-exceptions" } */
/* { dg-do compile } */

typedef volatile int IOSharedLockData;

@interface TestMyTests
- (void) testSpoon;
@end

extern void some_func (int *);

@implementation TestMyTests
- (void) testSpoon {
  int i = 5;

  do {
    @try {
      typeof(i) j = 6;
      some_func (&j);
    }
    @catch (id exc) {
      @throw;
    }
  } while(0);

  do {
    @try {
      typeof(i) j = 7;
      some_func (&j);
    }
    @catch (id exc) {
      @throw;
    }
  } while(0);

  do {
    @try {
      typeof(i) j = 8;
      some_func (&j);
    }
    @catch (id exc) {
      @throw;
    }
  } while(0);
      
}
@end
