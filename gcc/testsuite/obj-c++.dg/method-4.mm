/* Warn about "slightly" mismatched method signatures if 
   -Wstrict-selector-match is on.  */

/* { dg-do compile } */
/* { dg-options "-Wstrict-selector-match" } */

#include <objc/objc.h>

@interface Base
- (id) meth1: (Base *)arg1; /* { dg-warning "using .\\-\\(id\\)meth1:\\(Base \\*\\)arg1." } */
- (id) window;              /* { dg-warning "using .\\-\\(id\\)window" } */
@end

@interface Derived: Base
- (id) meth1: (Derived *)arg1; /* { dg-warning "also found .\\-\\(id\\)meth1:\\(Derived \\*\\)arg1." } */
- (Base *) window;             /* { dg-warning "also found .\\-\\(Base \\*\\)window." } */
@end

void foo(void) {
  id r;

  [r meth1:r];  /* { dg-warning "multiple methods named .\\-meth1:. found" } */
  [r window];   /* { dg-warning "multiple methods named .\\-window. found" } */
}
