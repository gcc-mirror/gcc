#define C L'\400'

#if C
#define zero (!C)
#else
#define zero C
#endif

main()
{
  if (zero != 0)
    abort ();
  exit (0);
}
