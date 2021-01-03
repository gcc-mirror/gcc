/* Do not warn about "slightly" mismatched method signatures if 
   -Wstrict-selector-match is off.  */

/* { dg-do compile } */
/* { dg-options "-Wno-strict-selector-match" } */
// { dg-additional-options "-Wno-objc-root-class" }

#include <objc/objc.h>

typedef enum { en1_1, en1_2 } En1;
typedef enum { en2_1, en2_2 } En2;
typedef struct { int a, b; } St1;
typedef struct { unsigned a, b; } St2;

@interface Base
- (id) meth1: (En1)arg1;
- (St1) window;
@end

@interface Derived: Base
- (id) meth1: (En2)arg1;
- (St2)window;
@end

void foo(void) {
  id r;
  En1 en;

  [r meth1:en];
  [r window];
}
