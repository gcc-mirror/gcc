typedef struct
{
}
__sigset_t;
typedef struct
{
    char coredump;
}
EMode;
extern EMode Mode;
struct sigaction
{
  __sigset_t sa_mask;
  int sa_flags;
};
void sigemptyset (__sigset_t *);
void
doSignalsSetup (void)
{
  static const int signals[] = {
    1, 2 , 3, 4, 6, 8, 11, 13, 14, 15, 10, 12, 17, 7
  };
  unsigned int i, sig;
  struct sigaction sa;
  for (i = 0; i < sizeof (signals) / sizeof (int); i++)
    {
      sig = signals[i];
      if (Mode.coredump && (sig == 4 || sig == 8))
        continue;
      sa.sa_flags = (sig == 17);
      sigemptyset (&sa.sa_mask);
    }
}
