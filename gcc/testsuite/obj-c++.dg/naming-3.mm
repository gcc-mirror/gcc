/* Testing for detecting duplicate ivars. */
/* { dg-do compile } */

@interface A 
{
  char x; /* { dg-error "conflicts" } */
  char x;
} /* { dg-error "declaration" } */
@end

@interface B : A
{
  char y;
}
@end
