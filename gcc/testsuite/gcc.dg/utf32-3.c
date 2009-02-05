/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test concatenation of char32_t* string literals. */
/* { dg-do run } */
/* { dg-xfail-run-if "PR36470" { "avr-*-*" "m32c-*-*" } { "*" } { "" } } */
/* { dg-options "-std=gnu99 -Wall -Werror" } */

typedef __CHAR32_TYPE__ char32_t;

extern void abort (void);

char32_t	*s0 = U"a" U"b";

char32_t	*s1 = U"a" "b";
char32_t	*s2 = "a" U"b";
char32_t	*s3 = U"a" "\u2029";
char32_t	*s4 = "\u2029" U"b";
char32_t	*s5 = U"a" "\U00064321";
char32_t	*s6 = "\U00064321" U"b";

#define A	0x00000061
#define B	0x00000062
#define X	0x00002029
#define Y	0x00064321

int main ()
{
    if (sizeof ((U"a" U"b")[0]) != sizeof (char32_t))
	abort ();
    if (sizeof ((U"a"  "b")[0]) != sizeof (char32_t))
	abort ();
    if (sizeof (( "a" U"b")[0]) != sizeof (char32_t))
	abort ();

    if (s0[0] != A || s0[1] != B || s0[2] != 0x00000000)
	abort ();

    if (s1[0] != A || s1[1] != B || s1[2] != 0x00000000)
	abort ();
    if (s2[0] != A || s2[1] != B || s2[2] != 0x00000000)
	abort ();
    if (s3[0] != A || s3[1] != X || s3[2] != 0x00000000)
	abort ();
    if (s4[0] != X || s4[1] != B || s4[2] != 0x00000000)
	abort ();
    if (s5[0] != A || s5[1] != Y || s5[2] != 0x00000000)
	abort ();
    if (s6[0] != Y || s6[1] != B || s6[2] != 0x00000000)
	abort ();
}
