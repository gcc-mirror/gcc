/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Ensure that a typedef to char16_t/char32_t is fine in c++98. */
/* { dg-do compile } */
/* { dg-options "-std=c++98" } */

typedef short unsigned int	char16_t;
typedef unsigned int		char32_t;
