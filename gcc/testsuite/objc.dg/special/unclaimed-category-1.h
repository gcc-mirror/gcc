/* Contributed by Nicola Pero - Fri Dec 14 08:36:00 GMT 2001 */

/* Test loading unclaimed categories - categories of a class defined
   separately from the class itself.  */

@interface TestClass
{
#ifdef __OBJC2__
  Class isa;
#else
  id isa;
#endif
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


