/* Contributed by Nicola Pero <nicola.pero@meta-innovation.com>, December 2010.  */

/* Test the order of calling +load between classes and categories.  */

void complete_load_step (int load_step);
void check_that_load_step_was_completed (int load_step);
void check_that_load_step_was_not_completed (int load_step);

@interface TestClass1
{
  id isa;
}
@end

@interface TestClass2 : TestClass1
@end

@interface TestClass3 : TestClass2
@end
