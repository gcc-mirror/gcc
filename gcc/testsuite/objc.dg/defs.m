/* Check if the @defs() construct preserves the correct
   offsets of ivars.  */
/* Contributed by Ziemowit Laski <zlaski@apple.com>.  */
/* { dg-options "-lobjc" } */
/* { dg-do run } */

#include <objc/objc.h>
#include <objc/Object.h>

extern void abort(void);

#define CHECK_IF(expr) if(!(expr)) abort();

@interface Base: Object {
@public
  int a;
  float b;
  char c;
}
@end

@interface Derived: Base {
@public
  double d;
  unsigned e;
  id f;
} 
- init;
@end

struct Derived_defs {
  @defs(Derived);
};

@implementation Base
@end
@implementation Derived
- init {
  [super init];
  a = 123;
  b = 1.23;
  c = 'c';
  d = 123.456;
  e = 456;
  f = isa;
  return self;
}
@end

int main(void) {
  Derived *derived = [[Derived alloc] init];
  struct Derived_defs *derived_defs = (struct Derived_defs *)derived;

  CHECK_IF(derived->a == derived_defs->a && derived_defs->a == 123);
  CHECK_IF(derived->b == derived_defs->b && derived_defs->b == (float)1.23);  
  CHECK_IF(derived->c == derived_defs->c && derived_defs->c == 'c');  
  CHECK_IF(derived->d == derived_defs->d && derived_defs->d == (double)123.456);  
  CHECK_IF(derived->e == derived_defs->e && derived_defs->e == 456);  
  CHECK_IF(derived->f == derived_defs->f && derived_defs->f == derived_defs->isa);

  /* Try out the "inline" notation as well.  */
  CHECK_IF(((struct { @defs(Derived); } *)derived)->a == 123);
  CHECK_IF(((struct { @defs(Derived); } *)derived)->c == 'c');
  CHECK_IF(((struct { @defs(Derived); } *)derived)->e == 456);

  return 0;
}
