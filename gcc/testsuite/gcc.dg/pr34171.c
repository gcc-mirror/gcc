/* Testcase by Martin Michlmayr <tbm@cyrius.com> */

/* { dg-do compile } */
/* { dg-options "-O3" } */

extern char coredump;
extern void sigemptyset (char *);
struct sigaction
{
  char sa_mask;
};
void doSignalsSetup (void)
{
  static const int signals[] = {
    1, 2, 3, 4, 6, 8, 11, 13, 14, 15, 30 , 31
  };
  unsigned int i, sig;
  struct sigaction sa;
  for (i = 0; i < sizeof (signals) / sizeof (int); i++)
    {
      sig = signals[i];
      if (coredump &&
          (sig == 4 || sig == 8 || sig == 11 || sig == 10))
        continue;
      sigemptyset (&sa.sa_mask);
    }
}
