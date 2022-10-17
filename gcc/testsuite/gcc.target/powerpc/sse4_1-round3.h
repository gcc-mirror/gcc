#include <smmintrin.h>
#include <fenv.h>
#include "sse4_1-check.h"

#define DIM(a) (sizeof (a) / sizeof (a)[0])

static int roundings[] =
  {
    _MM_FROUND_TO_NEAREST_INT,
    _MM_FROUND_TO_NEG_INF,
    _MM_FROUND_TO_POS_INF,
    _MM_FROUND_TO_ZERO,
    _MM_FROUND_CUR_DIRECTION
  };

static int modes[] =
  {
    FE_TONEAREST,
    FE_UPWARD,
    FE_DOWNWARD,
    FE_TOWARDZERO
  };

static void
TEST (void)
{
  int i, j, ri, mi, round_save;

  round_save = fegetround ();
  for (mi = 0; mi < DIM (modes); mi++) {
    fesetround (modes[mi]);
    for (i = 0; i < DIM (data); i++) {
      for (ri = 0; ri < DIM (roundings); ri++) {
	union value guess;
	union value *current_answers = answers[ri];
	switch ( roundings[ri] ) {
	  case _MM_FROUND_TO_NEAREST_INT:
	    guess.x = ROUND_INTRIN (data[i].value1.x, data[i].value2.x,
				    _MM_FROUND_TO_NEAREST_INT);
	    break;
	  case _MM_FROUND_TO_NEG_INF:
	    guess.x = ROUND_INTRIN (data[i].value1.x, data[i].value2.x,
				    _MM_FROUND_TO_NEG_INF);
	    break;
	  case _MM_FROUND_TO_POS_INF:
	    guess.x = ROUND_INTRIN (data[i].value1.x, data[i].value2.x,
				    _MM_FROUND_TO_POS_INF);
	    break;
	  case _MM_FROUND_TO_ZERO:
	    guess.x = ROUND_INTRIN (data[i].value1.x, data[i].value2.x,
				    _MM_FROUND_TO_ZERO);
	    break;
	  case _MM_FROUND_CUR_DIRECTION:
	    guess.x = ROUND_INTRIN (data[i].value1.x, data[i].value2.x,
				    _MM_FROUND_CUR_DIRECTION);
	    switch ( modes[mi] ) {
	      case FE_TONEAREST:
		current_answers = answers_NEAREST_INT;
		break;
	      case FE_UPWARD:
		current_answers = answers_POS_INF;
		break;
	      case FE_DOWNWARD:
		current_answers = answers_NEG_INF;
		break;
	      case FE_TOWARDZERO:
		current_answers = answers_ZERO;
		break;
	    }
	    break;
	  default:
	    abort ();
	}
	for (j = 0; j < DIM (guess.f); j++)
	  if (guess.f[j] != current_answers[i].f[j])
	    abort ();
      }
    }
  }
  fesetround (round_save);
}
