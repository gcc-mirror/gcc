#include <math.h>

extern void abort (void);
extern void exit (int);

#define NEQ(a, b) (fabs((a) - (b)) > 0.000001)

#define DECL_FUNC(TYPE1, TYPE2, ROUND)              \
   __attribute__ ((noinline, noclone)) TYPE2        \
   convert_##TYPE1##_to_##TYPE2##_##ROUND (TYPE1 N) \
    {                                               \
      return ROUND (N);                             \
    }

#define DECL_ALL_ROUNDS_FOR(ROUND_FUNC) \
  DECL_FUNC(float, float, ROUND_FUNC)   \
  DECL_FUNC(double, double, ROUND_FUNC) \
  DECL_FUNC(double, int, ROUND_FUNC)    \
  DECL_FUNC(double, long, ROUND_FUNC)   \
  DECL_FUNC(float, int, ROUND_FUNC)     \
  DECL_FUNC(float, long, ROUND_FUNC)


DECL_ALL_ROUNDS_FOR(round)
DECL_ALL_ROUNDS_FOR(ceil)
DECL_ALL_ROUNDS_FOR(floor)
DECL_ALL_ROUNDS_FOR(trunc)
DECL_ALL_ROUNDS_FOR(nearbyint)

#define TEST_ROUND(TYPE1, TYPE2, N, N_R, ROUND)      \
  if (NEQ (convert_##TYPE1##_to_##TYPE2##_##ROUND (N), N_R)) \
    abort ();


int main () {

  /* Round */
  TEST_ROUND(double, double, -4.8, -5.0, round);
  TEST_ROUND(double, double, -4.2, -4.0, round);
  TEST_ROUND(double, double, 4.8, 5.0, round);
  TEST_ROUND(double, double, 4.2, 4.0, round);

  TEST_ROUND(double, int, -4.8, -5, round);
  TEST_ROUND(double, int, -4.2, -4, round);
  TEST_ROUND(double, int, 4.8, 5, round);
  TEST_ROUND(double, int, 4.2, 4, round);

  TEST_ROUND(double, long, -4.8, -5, round);
  TEST_ROUND(double, long, -4.2, -4, round);
  TEST_ROUND(double, long, 4.8, 5, round);
  TEST_ROUND(double, long, 4.2, 4, round);

  TEST_ROUND(float, long, -4.8, -5, round);
  TEST_ROUND(float, long, -4.2, -4, round);
  TEST_ROUND(float, long, 4.8, 5, round);
  TEST_ROUND(float, long, 4.2, 4, round);

  /* Ceil */
  TEST_ROUND(double, double, -4.8, -4.0, ceil);
  TEST_ROUND(double, double, -4.2, -4.0, ceil);
  TEST_ROUND(double, double, 4.8, 5.0, ceil);
  TEST_ROUND(double, double, 4.2, 5.0, ceil);

  TEST_ROUND(double, int, -4.8, -4, ceil);
  TEST_ROUND(double, int, -4.2, -4, ceil);
  TEST_ROUND(double, int, 4.8, 5, ceil);
  TEST_ROUND(double, int, 4.2, 5, ceil);

  TEST_ROUND(double, long, -4.8, -4, ceil);
  TEST_ROUND(double, long, -4.2, -4, ceil);
  TEST_ROUND(double, long, 4.8, 5, ceil);
  TEST_ROUND(double, long, 4.2, 5, ceil);

  TEST_ROUND(float, long, -4.8, -4, ceil);
  TEST_ROUND(float, long, -4.2, -4, ceil);
  TEST_ROUND(float, long, 4.8, 5, ceil);
  TEST_ROUND(float, long, 4.2, 5, ceil);

  /* Floor */
  TEST_ROUND(double, double, -4.8, -5.0, floor);
  TEST_ROUND(double, double, -4.2, -5.0, floor);
  TEST_ROUND(double, double, 4.8, 4.0, floor);
  TEST_ROUND(double, double, 4.2, 4.0, floor);

  TEST_ROUND(double, int, -4.8, -5, floor);
  TEST_ROUND(double, int, -4.2, -5, floor);
  TEST_ROUND(double, int, 4.8, 4, floor);
  TEST_ROUND(double, int, 4.2, 4, floor);

  TEST_ROUND(double, long, -4.8, -5, floor);
  TEST_ROUND(double, long, -4.2, -5, floor);
  TEST_ROUND(double, long, 4.8, 4, floor);
  TEST_ROUND(double, long, 4.2, 4, floor);

  TEST_ROUND(float, long, -4.8, -5, floor);
  TEST_ROUND(float, long, -4.2, -5, floor);
  TEST_ROUND(float, long, 4.8, 4, floor);
  TEST_ROUND(float, long, 4.2, 4, floor);

  /* Trunc */
  TEST_ROUND(double, double, -4.8, -4.0, trunc);
  TEST_ROUND(double, double, -4.2, -4.0, trunc);
  TEST_ROUND(double, double, 4.8, 4.0, trunc);
  TEST_ROUND(double, double, 4.2, 4.0, trunc);

  TEST_ROUND(double, int, -4.8, -4, trunc);
  TEST_ROUND(double, int, -4.2, -4, trunc);
  TEST_ROUND(double, int, 4.8, 4, trunc);
  TEST_ROUND(double, int, 4.2, 4, trunc);

  TEST_ROUND(double, long, -4.8, -4, trunc);
  TEST_ROUND(double, long, -4.2, -4, trunc);
  TEST_ROUND(double, long, 4.8, 4, trunc);
  TEST_ROUND(double, long, 4.2, 4, trunc);

  TEST_ROUND(float, long, -4.8, -4, trunc);
  TEST_ROUND(float, long, -4.2, -4, trunc);
  TEST_ROUND(float, long, 4.8, 4, trunc);
  TEST_ROUND(float, long, 4.2, 4, trunc);

  /* Nearbyint */
  TEST_ROUND(double, double, -4.8, -5.0, nearbyint);
  TEST_ROUND(double, double, -4.2, -4.0, nearbyint);
  TEST_ROUND(double, double, 4.8, 5.0, nearbyint);
  TEST_ROUND(double, double, 4.2, 4.0, nearbyint);

  TEST_ROUND(double, int, -4.8, -5, nearbyint);
  TEST_ROUND(double, int, -4.2, -4, nearbyint);
  TEST_ROUND(double, int, 4.8, 5, nearbyint);
  TEST_ROUND(double, int, 4.2, 4, nearbyint);

  TEST_ROUND(double, long, -4.8, -5, nearbyint);
  TEST_ROUND(double, long, -4.2, -4, nearbyint);
  TEST_ROUND(double, long, 4.8, 5, nearbyint);
  TEST_ROUND(double, long, 4.2, 4, nearbyint);

  TEST_ROUND(float, long, -4.8, -5, nearbyint);
  TEST_ROUND(float, long, -4.2, -4, nearbyint);
  TEST_ROUND(float, long, 4.8, 5, nearbyint);
  TEST_ROUND(float, long, 4.2, 4, nearbyint);

  exit(0);
}

