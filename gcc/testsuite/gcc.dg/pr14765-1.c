/* Empty statement expressions should get void type.  Bug 14765 from
   Serge Belyshev <belyshev@lubercy.com>.  */
/* { dg-do compile } */
/* { dg-options "" } */

int a;
void fun ()
{
	a = 0;
	a = ({}); /* { dg-error "not ignored" "void stmt expr" } */
}
