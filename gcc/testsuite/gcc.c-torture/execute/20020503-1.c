/* PR 6534 */
/* GCSE unified the two i<0 tests, but if-conversion to ui=abs(i) 
   insertted the code at the wrong place corrupting the i<0 test.  */

void abort (void);
static char *
inttostr (long i, char buf[128])
{
  unsigned long ui = i;
  char *p = buf + 127;
  *p = '\0';
  if (i < 0)
    ui = -ui;
  do
    *--p = '0' + ui % 10;
  while ((ui /= 10) != 0);
  if (i < 0)
    *--p = '-';
  return p;
}

int
main ()
{
  char buf[128], *p;

  p = inttostr (-1, buf);
  if (*p != '-')
    abort ();
  return 0;
}
