/* { dg-do compile } */
/* { dg-options "-Wextra" } */
void foo (void)
{
	if (0)
	  a: ; /* { dg-warning "empty body in an if-statement" } */


}
