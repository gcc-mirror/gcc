/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test the support for char16_t* string literals. */
/* { dg-do run } */
/* { dg-options "-std=gnu99 -Wall -Werror" } */

typedef __CHAR16_TYPE__ char16_t;

extern void abort (void);

char16_t	*s0 = u"ab";
char16_t	*s1 = u"a\u0024";
char16_t	*s2 = u"a\u2029";
char16_t	*s3 = u"a\U00064321";

#define A	0x0061
#define B	0x0062
#define D	0x0024
#define X	0x2029
#define Y1	0xD950
#define Y2	0xDF21

int main ()
{
    if (s0[0] != A || s0[1] != B || s0[2] != 0x0000)
	abort ();
    if (s1[0] != A || s1[1] != D || s0[2] != 0x0000)
	abort ();
    if (s2[0] != A || s2[1] != X || s0[2] != 0x0000)
	abort ();
    if (s3[0] != A || s3[1] != Y1 || s3[2] != Y2 || s3[3] != 0x0000)
	abort ();
}
