/* Origin: PR c/128 from Martin Sebor <sebor@roguewave.com>, adapted
   as a testcase by Joseph Myers <jsm28@cam.ac.uk>.
*/
/* Character arrays initialised by a string literal must have
   uninitialised elements zeroed.  This isn't clear in the 1990
   standard, but was fixed in TC2 and C99; see DRs #060, #092.
*/
extern void abort (void);

int
foo (void)
{
  char s[2] = "";
  return 0 == s[1];
}

char *t;

int
main (void)
{
  {
    char s[] = "x";
    t = s;
  }
  if (foo ())
    exit (0);
  else
    abort ();
}
