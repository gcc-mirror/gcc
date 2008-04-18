/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test unsupported concatenation of char16_t/char32_t* string literals. */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void	*s0	= u"a"  "b";
void	*s1	=  "a" u"b";
void	*s2	= u"a" U"b";	/* { dg-error "non-standard concatenation" } */
void	*s3	= U"a" u"b";	/* { dg-error "non-standard concatenation" } */
void	*s4	= u"a" L"b";	/* { dg-error "non-standard concatenation" } */
void	*s5	= L"a" u"b";	/* { dg-error "non-standard concatenation" } */
void	*s6	= u"a" u"b";
void	*s7	= U"a"  "b";
void	*s8	=  "a" U"b";
void	*s9	= U"a" L"b";	/* { dg-error "non-standard concatenation" } */
void	*sa	= L"a" U"b";	/* { dg-error "non-standard concatenation" } */
void	*sb	= U"a" U"b";
void	*sc	= L"a"  "b";
void	*sd	=  "a" L"b";
void	*se	= L"a" L"b";

int main () {}
