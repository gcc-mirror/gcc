/* Redeclaration of parameters is an error.  PR 13728.  */
/* { dg-do compile } */

void f (int fred,	/* { dg-error "previous definition" "" } */
	int fred);	/* { dg-error "redefinition of parameter" "" } */

void f2 (int fred,	/* { dg-error "previous definition" "" } */
	 int fred)	/* { dg-error "redefinition of parameter" "" } */
{
}
