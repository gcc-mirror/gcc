/* Test warning for non existing selectors.  */
/* Contributed by Devang Patel <dpatel@apple.com>.  */
/* { dg-options "-Wselector -fnext-runtime" } */
/* { dg-do compile } */

typedef struct objc_object { struct objc_class *class_pointer; } *id;
typedef struct objc_selector    *SEL;

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
  a = @selector(b1ar); /* { dg-warning "creating selector for non existant method b1ar" } */
  b = @selector(bar);
}
@end

