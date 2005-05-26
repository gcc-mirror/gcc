/* Check for warnings about missing [super dealloc] calls.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile } */

@interface Foo {
  void *isa;
}
- (void) dealloc;
- (void) some_other;
@end

@interface Bar: Foo {
  void *casa;
}
- (void) dealloc0;
@end

@interface Baz: Bar {
  void *usa;
}
- (void) dealloc;
@end

@implementation Foo
- (void) dealloc {
  isa = 0;   /* Should not warn here.  */
}
- (void) some_other {
  isa = (void *)-1;
}
@end

@implementation Bar
- (void) dealloc0 {
  casa = 0;
  [super some_other];  /* Should not warn here.  */
} 
@end

@implementation Baz
- (void) dealloc {
  usa = 0;
  [super dealloc0]; 
} /* { dg-warning "method possibly missing a .super dealloc. call" } */
@end
