/* Test unsupported concatenation of UTF-8 string literals. */
/* { dg-do compile } */
/* { dg-options "-std=gnu99" } */

void	*s0	= u8"a"   "b";
void	*s1	=   "a" u8"b";
void	*s2	= u8"a" u8"b";
void	*s3	= u8"a"  u"b";	/* { dg-error "non-standard concatenation" } */
void	*s4	=  u"a" u8"b";	/* { dg-error "non-standard concatenation" } */
void	*s5	= u8"a"  U"b";	/* { dg-error "non-standard concatenation" } */
void	*s6	=  U"a" u8"b";	/* { dg-error "non-standard concatenation" } */
void	*s7	= u8"a"  L"b";	/* { dg-error "non-standard concatenation" } */
void	*s8	=  L"a" u8"b";	/* { dg-error "non-standard concatenation" } */

int main () {}
