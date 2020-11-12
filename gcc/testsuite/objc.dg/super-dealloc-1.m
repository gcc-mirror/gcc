/* Check for warnings about missing [super dealloc] calls.  */
/* Author: Ziemowit Laski <zlaski@apple.com>  */

/* { dg-do compile } */
/* { dg-additional-options "-Wno-objc-root-class" } */

@interface Foo {
  void *isa;
}
- (void) dealloc;
- (void) some_other;
@end

@interface Bar: Foo {
  void *casa;
}
- (void) dealloc;
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
- (void) dealloc {
  casa = 0;
  [super some_other];
}  /* { dg-warning "method possibly missing a .super dealloc. call" } */
@end

@implementation Baz
- (void) dealloc {
  usa = 0;
  [super dealloc];  /* Should not warn here.  */
}
@end
