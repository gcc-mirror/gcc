/* { dg-additional-options "--param analyzer-max-enodes-per-program-point=0 -Wno-analyzer-too-complex" } */

typedef void (*sighandler_t) (int);

void
signal (int, sighandler_t);

static void
kw (int signum)
{
  (void) signum;
}

void
gk (int ot)
{
  signal (ot, kw);
}
