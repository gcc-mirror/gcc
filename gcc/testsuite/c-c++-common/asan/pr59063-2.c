/* { dg-do run { target { *-*-linux* } } } */
/* { dg-options "-static-libasan" } */

#include <time.h>
static int weak_gettime (clockid_t clk_id, struct timespec *tp)
  __attribute__((__weakref__("clock_gettime")));
int main() {
  if (!clock_gettime)
    return 0;
  struct timespec ts;
  return weak_gettime(CLOCK_MONOTONIC, &ts);
}
