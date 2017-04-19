
/* Ensure that we indeed cannot obtain the value of a message send
   if the chosen method signature returns 'void'.  There used to
   exist a cheesy hack that allowed it.  While at it, check that
   the first lexically occurring method signature gets picked
   when sending messages to 'id'.  */ 
/* Contributed by Ziemowit Laski <zlaski@apple.com>  */
/* { dg-do compile } */

#include <objc/objc.h>

@interface Object1
- (void)initWithData:(Object1 *)data;
@end

@interface Object2
- (id)initWithData:(Object1 *)data;
@end

@interface Object3
- (id)initWithData:(Object2 *)data;
@end

void foo(void) {
  id obj1, obj2 = 0;
  obj2 = [obj1 initWithData: obj2];
     /* { dg-warning "multiple methods named .\\-initWithData:. found" "" { target *-*-* } .-1 } */
     /* { dg-message "using .\\-\\(void\\)initWithData:\\(Object1 \\*\\)data." "" { target *-*-* } 13 } */
     /* { dg-message "also found .\\-\\(id\\)initWithData:\\(Object1 \\*\\)data." "" { target *-*-* } 17 } */
     /* { dg-message "also found .\\-\\(id\\)initWithData:\\(Object2 \\*\\)data." "" { target *-*-* } 21 } */

     /* The following error is a consequence of picking the "wrong" method signature.  */
     /* { dg-error "void value not ignored as it ought to be" "" { target *-*-* } 26 } */
}
