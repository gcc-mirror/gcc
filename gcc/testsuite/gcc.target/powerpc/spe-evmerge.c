/* Verify SPE vector permute builtins.  */
/* { dg-do run { target { powerpc*-*-* && powerpc_spe } } } */
/* Remove `-ansi' from options so that <spe.h> compiles.  */
/* { dg-options "" } */

#include <spe.h>
#include <stdlib.h>

#define vector __attribute__ ((vector_size (8)))

#define WORDS_BIG_ENDIAN (__BYTE_ORDER__ == __ORDER_BIG_ENDIAN__)

int
main (void)
{
  vector int a = { 0x11111111, 0x22222222 };
  vector int b = { 0x33333333, 0x44444444 };
  vector int c;

  /* c[hi] = a[hi], c[lo] = b[hi]  */
  c = __ev_mergehi (a, b);
  if (c[0] != (WORDS_BIG_ENDIAN ? 0x11111111 : 0x44444444))
    abort ();
  if (c[1] != (WORDS_BIG_ENDIAN ? 0x33333333 : 0x22222222))
    abort ();
  /* c[hi] = a[lo], c[lo] = b[lo]  */
  c = __ev_mergelo (a, b);
  if (c[0] != (WORDS_BIG_ENDIAN ? 0x22222222 : 0x33333333))
    abort ();
  if (c[1] != (WORDS_BIG_ENDIAN ? 0x44444444 : 0x11111111))
    abort ();
  /* c[hi] = a[lo], c[lo] = b[hi]  */
  c = __ev_mergelohi (a, b);
  if (c[0] != (WORDS_BIG_ENDIAN ? 0x22222222 : 0x44444444))
    abort ();
  if (c[1] != (WORDS_BIG_ENDIAN ? 0x33333333 : 0x11111111))
    abort ();
  /* c[hi] = a[hi], c[lo] = b[lo]  */
  c = __ev_mergehilo (a, b);
  if (c[0] != (WORDS_BIG_ENDIAN ? 0x11111111 : 0x33333333))
    abort ();
  if (c[1] != (WORDS_BIG_ENDIAN ? 0x44444444 : 0x22222222))
    abort ();

  /* c[hi] = a[hi], c[lo] = b[hi]  */
  c = __builtin_spe_evmergehi (a, b);
  if (c[0] != (WORDS_BIG_ENDIAN ? 0x11111111 : 0x44444444))
    abort ();
  if (c[1] != (WORDS_BIG_ENDIAN ? 0x33333333 : 0x22222222))
    abort ();
  /* c[hi] = a[lo], c[lo] = b[lo]  */
  c = __builtin_spe_evmergelo (a, b);
  if (c[0] != (WORDS_BIG_ENDIAN ? 0x22222222 : 0x33333333))
    abort ();
  if (c[1] != (WORDS_BIG_ENDIAN ? 0x44444444 : 0x11111111))
    abort ();
  /* c[hi] = a[lo], c[lo] = b[hi]  */
  c = __builtin_spe_evmergelohi (a, b);
  if (c[0] != (WORDS_BIG_ENDIAN ? 0x22222222 : 0x44444444))
    abort ();
  if (c[1] != (WORDS_BIG_ENDIAN ? 0x33333333 : 0x11111111))
    abort ();
  /* c[hi] = a[hi], c[lo] = b[lo]  */
  c = __builtin_spe_evmergehilo (a, b);
  if (c[0] != (WORDS_BIG_ENDIAN ? 0x11111111 : 0x33333333))
    abort ();
  if (c[1] != (WORDS_BIG_ENDIAN ? 0x44444444 : 0x22222222))
    abort ();

  return 0;
}
