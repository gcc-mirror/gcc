/* Testing for detecting duplicate ivars. */
/* { dg-do compile } */

@interface A 
{
  /* TODO: Have the testsuite check that these messages occur only once!  */
  char x; /* { dg-message "previous declaration" } */
  char x; /* { dg-error "duplicate instance variable" } */
}
@end

/* In some versions of the compiler (eg, 4.6.x), having a subclass
   would generate additional, duplicate errors for the duplicate
   instance variable in the superclass, so adding the following would
   cause the error messages above to be duplicated.  */
@interface B : A
{
  char y;
}
@end
