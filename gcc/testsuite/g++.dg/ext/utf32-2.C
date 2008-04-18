/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test the support for char32_t* string constants. */
/* { dg-do run } */
/* { dg-options "-std=c++0x -Wall -Werror" } */

extern "C" void abort (void);

const static char32_t	*s0 = U"ab";
const static char32_t	*s1 = U"a\u0024";
const static char32_t	*s2 = U"a\u2029";
const static char32_t	*s3 = U"a\U00064321";

#define A	0x00000061
#define B	0x00000062
#define D	0x00000024
#define X	0x00002029
#define Y	0x00064321

int main ()
{
    if (s0[0] != A || s0[1] != B || s0[2] != 0x00000000)
	abort ();
    if (s1[0] != A || s1[1] != D || s0[2] != 0x00000000)
	abort ();
    if (s2[0] != A || s2[1] != X || s0[2] != 0x00000000)
	abort ();
    if (s3[0] != A || s3[1] != Y || s3[2] != 0x00000000)
	abort ();
}
