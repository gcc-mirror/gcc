/* Testing for detecting duplicate ivars. */
/* { dg-do compile } */

@interface A 
{
  char x; /* { dg-message "previous declaration" } */
  char x;
} /* { dg-error "redeclaration" } */
@end

@interface B : A
{
  char y;
}
@end
