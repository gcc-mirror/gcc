/* { dg-do run } */

/* Regression test for stringizing and token pasting.
   We got internal escape markers in the strings.  */

#include <string.h>
#include <stdlib.h>

#define S(x) _S(x)
#define _S(x) #x

#define I 1
static const char s1[] = S(I.1);
static const char t1[] = "1.1";

#define f h
#define h(a) a+f
static const char s2[] = S( f(1)(2) );
static const char t2[] = "1+h(2)";

#undef I
#undef f
#undef h

int
main(void)
{
  if (strcmp (s1, t1))
    abort ();

  if (strcmp (s2, t2))
    abort ();

  return 0;
}
