/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Test the support for char16_t character constants. */
/* { dg-do run } */
/* { dg-options "-std=c++0x -Wall -Werror" } */

extern "C" void abort (void);

const static char16_t	c0 = u'a';
const static char16_t	c1 = u'\0';
const static char16_t	c2 = u'\u0024';
const static char16_t	c3 = u'\u2029';
const static char16_t	c4 = u'\u8010';

const static char16_t	c5 = 'a';
const static char16_t	c6 = U'a';
const static char16_t	c7 = U'\u2029';
const static char16_t	c8 = U'\u8010';
const static char16_t	c9 = L'a';
const static char16_t	ca = L'\u2029';
const static char16_t	cb = L'\u8010';

#define A	0x0061
#define D	0x0024
#define X	0x2029
#define Y	0x8010

int main ()
{
    if (sizeof (u'a') != sizeof (char16_t))
	abort ();
    if (sizeof (u'\0') != sizeof (char16_t))
	abort ();
    if (sizeof (u'\u0024') != sizeof (char16_t))
	abort ();
    if (sizeof (u'\u2029') != sizeof (char16_t))
	abort ();
    if (sizeof (u'\u8010') != sizeof (char16_t))
	abort ();

    if (c0 != A)
	abort ();
    if (c1 != 0x0000)
	abort ();
    if (c2 != D)
	abort ();
    if (c3 != X)
	abort ();
    if (c4 != Y)
	abort ();

    if (c5 != A)
	abort ();
    if (c6 != A)
	abort ();
    if (c7 != X)
	abort ();
    if (c8 != Y)
	abort ();
    if (c9 != A)
	abort ();
    if (ca != X)
	abort ();
    if (cb != Y)
	abort ();
}
