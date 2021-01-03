/* Test Wobjc-root-class warning is suppressed by the objc_root_class attr.
   Note that we don't issue a warning unless the TU contains an implementation
   for the class.  This should compile without warning.  */
/*  { dg-additional-options "-fsyntax-only " } */

__attribute__((objc_root_class))
@interface ARootObject
@end

@implementation ARootObject
@end
