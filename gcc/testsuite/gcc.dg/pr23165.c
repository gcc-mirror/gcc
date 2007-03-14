/* { dg-do compile } */
/* { dg-options "-Wempty-body" } */
void foo (void)
{
	if (0)
	  a: ; /* { dg-warning "empty body in an" } */


}
