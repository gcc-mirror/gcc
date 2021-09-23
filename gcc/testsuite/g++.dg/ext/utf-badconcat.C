/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test unsupported concatenation of char16_t/char32_t* string literals. */
/* { dg-do compile { target c++11 } } */

const void *s0	= u"a"  "b";
const void *s1	=  "a" u"b";
const void *s2	= u"a" U"b";	/* { dg-error "concatenation" } */
const void *s3	= U"a" u"b";	/* { dg-error "concatenation" } */
const void *s4	= u"a" L"b";	/* { dg-error "concatenation" } */
const void *s5	= L"a" u"b";	/* { dg-error "concatenation" } */
const void *s6	= u"a" u"b";
const void *s7	= U"a"  "b";
const void *s8	=  "a" U"b";
const void *s9	= U"a" L"b";	/* { dg-error "concatenation" } */
const void *sa	= L"a" U"b";	/* { dg-error "concatenation" } */
const void *sb	= U"a" U"b";
const void *sc	= L"a"  "b";
const void *sd	=  "a" L"b";
const void *se	= L"a" L"b";

int main () {}
