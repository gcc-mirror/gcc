/* On h8300 port, the following used to be broken with -mh or -ms.  */

extern void abort (void);
extern void exit (int);

unsigned long
foo (unsigned long a)
{
  return a ^ 0x0000ffff;
}

unsigned long
bar (unsigned long a)
{
  return a ^ 0xffff0000;
}

int
main ()
{
  if (foo (0) != 0x0000ffff)
    abort ();

  if (bar (0) != 0xffff0000)
    abort ();

  exit (0);
}
