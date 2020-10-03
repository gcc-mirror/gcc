/* { dg-do compile } */
/* { dg-options "-O -fdiagnostics-show-caret" } */

/* This file is similar to diagnostic-test-expressions-1.c
   (see the notes in that file); this file adds test
   coverage for various Objective C constructs. */

#include <objc/runtime.h> /* for SEL, Protocol */

extern void __emit_expression_range (int dummy, ...);

@protocol prot
@end

@interface tests <prot>
- (int) func0;
- (int) func1:(int)i;
+ (int) func2;
- (void) test_sending_messages;
+ (void) test_class_dot_name;
- (void) test_at_selector;
- (void) test_at_protocol;
- (void) test_at_encode:(int)i;
@end

@implementation tests
- (int) func0
{
  return 42;
}
- (int) func1:(int)i
{
  return i * i;
}
+ (int) func2
{
  return 0;
}
- (void) test_sending_messages
{
  __emit_expression_range ( 0, [self func0] ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range ( 0, [self func0] );
                                ~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
  __emit_expression_range ( 0, [self func1:5] ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range ( 0, [self func1:5] );
                                ~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}
+ (void) test_class_dot_name
{
  __emit_expression_range ( 0, tests.func2 ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range ( 0, tests.func2 );
                                ~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

- (void) test_at_selector
{
  /* For the NeXT runtime, @selector() generates a a var decl which (a) isn't
     handled by the plugin, and (b) if it was would not necessarily have the
     right location (there is only one var decl uniqued to each selector 
     spelling, so the location would be that of the first occurrence).  Use an
     assignment expression to test the operation.   */
  SEL aSel;
  __emit_expression_range ( 0, aSel = @selector(foo) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range ( 0, aSel = @selector(foo) );
                                ~~~~~^~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}
- (void) test_at_protocol
{
  /* As for @selector(), the NeXT runtime generates a a var decl for
     @protocol() handle this in a similar way.  */
  Protocol *aProt;
  __emit_expression_range ( 0, aProt = @protocol(prot) ); /* { dg-warning "range" "" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range ( 0, aProt = @protocol(prot) );
                                ~~~~~~^~~~~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}

- (void) test_at_encode:(int)i
{
  /* @encode() generates a STRING_CST which doesn't retain a location
     after parsing, so we need to access it via compound expressions
     that can't be folded away.  */

  /* Verify start.  */
  __emit_expression_range ( 0, @encode(int) + i ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range ( 0, @encode(int) + i );
                                ~~~~~~~~~~~~~^~~
   { dg-end-multiline-output "" } */

  /* Verify finish.  */
  __emit_expression_range ( 0, i + @encode(int) ); /* { dg-warning "range" } */
/* { dg-begin-multiline-output "" }
   __emit_expression_range ( 0, i + @encode(int) );
                                ~~^~~~~~~~~~~~~~
   { dg-end-multiline-output "" } */
}
@end
