/* { dg-do compile { target nonpic } }
   { dg-options "-O2 -Wsuggest-attribute=returns_nonnull" } */

int *q;
int *test()    /* { dg-warning "candidate for attribute .returns_nonnull." } */
{
	if (!q)
		__builtin_unreachable ();
	return q;
}
