/* Check if objc_super stack variables are created correctly (and
   not clobbered by other values).  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-std=c99" } */
/* { dg-do run } */
/* { dg-xfail-run-if "Needs OBJC2 ABI" { *-*-darwin* && { lp64 && { ! objc2 } } } { "-fnext-runtime" } { "" } } */

#include "../objc-obj-c++-shared/Object1.h"

extern void abort(void);

#define CHECK_IF(expr) if(!(expr)) abort();

typedef struct _Point { 
  float x; 
  float y; 
} Point; 

Point MakePoint ( float x , float y ) { 
  Point p; 
  p.x = x; 
  p.y = y; 
  return p; 
} 

@interface Base: Object 
- ( void ) translateOriginToPoint : ( Point ) translation ; 
@end

@interface Derived : Base 
- ( void ) scrollToPoint : ( Point ) newOrigin ; 
- ( void ) translateOriginToPoint : ( Point ) translation ;
@end 

int blort;
float result;

@implementation Base
- ( void ) translateOriginToPoint : ( Point ) translation  {
  result = translation.x + translation.y;
}
@end

@implementation Derived
- ( void ) scrollToPoint : ( Point ) newOrigin { 
  float transDeltaX =newOrigin.x, transDeltaY =newOrigin.y ; 
  Point w;
  if ( ! blort ) {
    w.x = transDeltaX ; w.y = transDeltaY ;
    [ super translateOriginToPoint : w ] ; 
    return;
  } 
  [ super translateOriginToPoint : MakePoint ( transDeltaX , transDeltaY ) ] ; 
  return; 
} 
- (void) translateOriginToPoint : ( Point ) translation  {
  /* This should never be called.  */
  CHECK_IF(0);
}
@end 

int main(void) {
  Derived *v = [Derived new];
  float r0 = 1.5 + 1.5;
  blort = 1;
  [v scrollToPoint: MakePoint(1.5, 1.5)];
  CHECK_IF(result == r0);
  blort = 0;
  [v scrollToPoint: MakePoint(1.5, 1.5)];
  CHECK_IF(result == r0);
  blort = 1;
  [v scrollToPoint: MakePoint(1.5, 1.5)];
  CHECK_IF(result == r0);
  [v free];
  return 0;
}

#include "../objc-obj-c++-shared/Object1-implementation.h"
