void abort (void);
extern int a;

__attribute__ ((noinline))
void
clone_me (int *ptr)
{
  if (ptr != &a)
    abort ();
  if (!__builtin_constant_p (ptr != &a))
    abort ();
}
