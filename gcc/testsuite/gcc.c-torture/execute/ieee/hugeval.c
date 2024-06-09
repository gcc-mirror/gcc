#include <math.h>

void abort (void);
void exit (int);

static const double zero =  0.0;
static const double pone =  1.0;
static const double none = -1.0;
static const double pinf =  1.0 / 0.0;
static const double ninf = -1.0 / 0.0;

int
main ()
{
  if (pinf != pone/zero)
    abort ();

  if (ninf != none/zero)
    abort ();

#ifdef HUGE_VAL
  if (HUGE_VAL != pinf)
    abort ();

  if (-HUGE_VAL != ninf)
    abort ();
#endif

  exit (0);
}
