/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */

/* Test that +load works when a category is defined in a different
   module than the main class.  */

/* This function should be called any time +load is invoked, so we can
   keep the count.  */
extern int increase_load_count (void);

@interface TestClass1
{
  id isa;
}
@end

@interface TestClass2
{
  id isa;
}
@end
