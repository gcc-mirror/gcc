/* { dg-skip-if "requires hosted libstdc++ for stdlib malloc" { ! hostedlib } } */

#include <stdlib.h>

extern int foo (void);

int successes;
int failures;

#define ONE_DIAMOND \
    do {                                                \
      void *ptr = malloc (128);				\
      if (foo ())					\
	successes++;					\
      else						\
	failures++;					\
      free (ptr);					\
    } while (0)

#define TEN_DIAMONDS \
  do {								   \
    ONE_DIAMOND; ONE_DIAMOND; ONE_DIAMOND; ONE_DIAMOND; ONE_DIAMOND;	\
    ONE_DIAMOND; ONE_DIAMOND; ONE_DIAMOND; ONE_DIAMOND; ONE_DIAMOND; \
 } while (0)

void test_3 (void *ptr)
{
  free (ptr);
#if 1
  ONE_DIAMOND;
#else
  /* TODO: enabling this leads to numerous duplicated reports,
     all of them detailing all the extraneous info about the malloc/free
     within the diamonds.  */
  TEN_DIAMONDS;
#endif
  free (ptr); /* { dg-warning "double-'free' of 'ptr'" } */
}
