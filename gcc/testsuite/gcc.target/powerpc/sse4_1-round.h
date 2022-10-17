#include <fenv.h>
#include <smmintrin.h>
#include "sse4_1-check.h"

#define DIM(a) (sizeof (a) / sizeof (a)[0])

static int modes[] = { FE_TONEAREST, FE_UPWARD, FE_DOWNWARD, FE_TOWARDZERO };

static void
TEST (void)
{
  int i, j, ri, round_save;

  round_save = fegetround ();
  for (ri = 0; ri < DIM (modes); ri++) {
    fesetround (modes[ri]);
    for (i = 0; i < DIM (data); i++) {
      union value guess;
      guess.x = ROUND_INTRIN (data[i].value.x, /* Ignored.  */);
      for (j = 0; j < DIM (data[i].value.f); j++) {
        if (guess.f[j] != data[i].answer[j])
          abort ();
      }
    }
  }
  fesetround (round_save);
}
