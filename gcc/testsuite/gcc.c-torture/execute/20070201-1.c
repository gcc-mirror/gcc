/* PR middle-end/30473 */

extern int sprintf (char *, const char *, ...);
extern void abort (void);

char *
foo (char *buf, char *p)
{
  sprintf (buf, "abcde", p++);
  return p;
}

int
main (void)
{
  char buf[6];
  if (foo (buf, &buf[2]) != &buf[3])
    abort ();
  return 0;
}
