/* Test warning for non-existent selectors.  */
/* This is the "-fgnu-runtime" variant of objc.dg/selector-1.m.  */
/* { dg-options "-Wselector -fgnu-runtime" } */
/* { dg-do compile } */

typedef struct objc_object { struct objc_class *class_pointer; } *id;
typedef const struct objc_selector    *SEL;

@interface Foo
- (void) foo;
- (void) bar;
@end

@implementation Foo
- (void) bar
{
}

- (void) foo
{
  SEL a,b,c;
  a = @selector(b1ar);
  b = @selector(bar);
}
@end /* { dg-warning "creating selector for nonexistent method .b1ar." } */

