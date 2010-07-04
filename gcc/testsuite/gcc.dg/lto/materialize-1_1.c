int inline_me (void);
extern int a;
void abort (void);

__attribute__ ((noinline))
void
clone_me (int c, int d)
{
  if (!c)
    {
      if (d!=a)
	abort ();
    }
}
int
main(void)
{
  int i;
  for (i=0;i<a;i++)
    inline_me ();
  return 0;
}
