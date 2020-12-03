/* Test Wobjc-root-class.
   Note that we don't issue a warning unless the TU contains an implementation
   for the class.  */
/*  { dg-additional-options "-fsyntax-only " } */

@interface ARootObject
@end

@implementation ARootObject
@end /* { dg-warning {class 'ARootObject' defined without specifying a base class} } */
