/* Contributed by Nicola Pero - Fri Dec 14 08:36:00 GMT 2001 */
/* { dg-additional-options "-Wno-objc-root-class" } */

/* Test loading unclaimed categories - categories of a class defined
   separately from the class itself.  */

@interface TestClass
{
  Class isa;
}
- (int)D;
@end

@interface TestClass (A)
- (int)A;
@end

@interface TestClass (B)
- (int)B;
@end

@interface TestClass (C)
- (int)C;
@end


