/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Ensure that a typedef to char16_t/char32_t issues a warning in c++0x. */
/* { dg-do compile } */
/* { dg-options "-std=c++0x" } */

typedef short unsigned int	char16_t; /* { dg-warning "redeclaration" } */
typedef unsigned int		char32_t; /* { dg-warning "redeclaration" } */
