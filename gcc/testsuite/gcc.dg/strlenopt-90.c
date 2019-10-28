/* PR tree-optimization/92226 - live nul char store to array eliminated
   { dg-do compile }
   { dg-options "-O2 -fdump-tree-strlen" } */

#include "strlenopt.h"

#define NOIPA __attribute__ ((noipa))

#define T(MIN, MAX, SIZE, IDX)						\
  NOIPA void								\
  test_ ## MIN ## _ ## MAX ## _ ## SIZE ## _ ## IDX (const char *s)	\
  {									\
    extern char a ## SIZE[SIZE];					\
    char *d = a ## SIZE;						\
    size_t len = strlen (s);						\
    size_t idx = IDX;							\
    if (MIN <= len && len <= MAX)					\
      {									\
	strcpy (d, s);							\
	d[idx] = 0;							\
	if (strlen (d) != idx)						\
	  abort ();							\
      }									\
  } typedef void dummy_type


/* The final nul store must be retained but the second strlen call should
   be eliminated because the final length of the destination after the nul
   store must be equal to the index of the store.  */
T (0, 2, 4, 0);

/* Not handled yet (see below):
   T (0, 2, 4, 1);  */

/* Not handled yet: in addition to the cases above, the second strlen
   call can also be eliminated in those below because in both the final
   length of the destination after the nul store must be in the same
   range as the length of the source.
   T (0, 2, 4, 2);
   T (0, 2, 4, 3);  */

T (2, 3, 4, 0);
T (2, 3, 4, 1);

/* Not handled yet (see above):
   T (2, 3, 4, 2);
   T (2, 3, 4, 3);  */

T (3, 4, 5, 0);
T (3, 4, 5, 1);
T (3, 4, 5, 2);

/* Not handled yet (see above):
   T (3, 4, 5, 3);
   T (3, 4, 5, 4);  */

T (3, 4, 6, 0);
T (3, 4, 6, 1);
T (3, 4, 6, 2);

/* Not handled yet (see above):
   T (3, 4, 6, 3);
   T (3, 4, 6, 4);
   T (3, 4, 6, 5);  */


/* Verify that each function makes just one call to strlen to compute
   the length of its argument (and not also to compute the length of
   the copy):
  { dg-final { scan-tree-dump-times "strlen \\(s_" 9 "strlen1" } }
  { dg-final { scan-tree-dump-not "strlen \\(\\&a" "strlen1" } }

  Verify that nul stores into the destination have not been eliminated:
  { dg-final { scan-tree-dump-times "a4\\\] = 0;" 2 "strlen1" } }
  { dg-final { scan-tree-dump-times "a4 \\\+ 1B\\\] = 0;" 1 "strlen1" } }

  { dg-final { scan-tree-dump-times "a5\\\] = 0;" 1 "strlen1" } }
  { dg-final { scan-tree-dump-times "a5 \\\+ 1B\\\] = 0;" 1 "strlen1" } }
  { dg-final { scan-tree-dump-times "a5 \\\+ 2B\\\] = 0;" 1 "strlen1" } }

  { dg-final { scan-tree-dump-times "a6\\\] = 0;" 1 "strlen1" } }
  { dg-final { scan-tree-dump-times "a6 \\\+ 1B\\\] = 0;" 1 "strlen1" } }
  { dg-final { scan-tree-dump-times "a6 \\\+ 2B\\\] = 0;" 1 "strlen1" } }  */
