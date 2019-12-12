/* Ensure that __CHAR8_TYPE__ exists and matches the underlying type. */
/* { dg-do run { target c++11 } } */
/* { dg-options "-fchar8_t -Wall -Werror" } */

extern "C" void abort (void);

int main ()
{
    if (sizeof (__CHAR8_TYPE__) != sizeof (char8_t))
	abort();
}
