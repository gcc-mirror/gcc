#ifdef __i386__
f (rp)
     unsigned int *rp;
{
  __asm__ ("mull %3" : "=a" (rp[0]), "=d" (rp[1]) : "%0" (7), "rm" (7));
}

main ()
{
  unsigned int s[2];

  f (s);
  if (s[1] != 0 || s[0] != 49)
    abort ();
 exit (0);
}
#else
main ()
{
  exit (0);
}
#endif
