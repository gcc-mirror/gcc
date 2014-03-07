/* Contributed by Kris Van Hees <kris.van.hees@oracle.com> */
/* Ensure that __CHAR16_TYPE__ and __CHAR32_TYPE__ exist, match the types they
   are the underlying data type for. */
/* { dg-do run { target c++11 } } */
/* { dg-options "-Wall -Werror" } */

extern "C" void abort (void);

int main ()
{
    if (sizeof (__CHAR16_TYPE__) != sizeof (char16_t))
	abort();
    if (sizeof (__CHAR32_TYPE__) != sizeof (char32_t))
	abort();
}
