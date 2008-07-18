/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Ensure that __CHAR16_TYPE__ and __CHAR32_TYPE__ exist, and are of the
   correct width. */
/* { dg-do run } */
/* { dg-options "-std=gnu99 -Wall -Werror" } */

typedef __CHAR16_TYPE__ char16_t;
typedef __CHAR32_TYPE__ char32_t;

extern void abort (void);

int main ()
{
    if (sizeof (char16_t) != sizeof (u'a'))
	abort();
    if (sizeof (char32_t) != sizeof (U'a'))
	abort();
}
