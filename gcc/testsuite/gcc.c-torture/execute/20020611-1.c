/* PR target/6997.  Missing (set_attr "cc" "none") in sleu pattern in
   cris.md.  Testcase from hp@axis.com.  */

void abort (void);
void exit (int);

int p;
int k;
unsigned int n;

void x ()
{
  unsigned int h;

  h = n <= 30;
  if (h)
    p = 1;
  else
    p = 0;

  if (h)
    k = 1;
  else
    k = 0;
}

unsigned int n = 30;

int
main (void)
{
  x ();
  if (p != 1 || k != 1)
    abort ();
  exit (0);
}
