/* { dg-do run } */

/* Test that, when a macro expansion spills into the source file, we
   expand macros we suck in from there, as the standard clearly states
   they are not nested.

   Submitter: Neil Booth, with minor modifications to the originals. 3
   Dec 2000.
   Source: PR 962 and Thomas Pornin.  */

extern void abort (void);
int e = 10, f = 100;

#define e(n) 4 + n
#define X e
#define f(x) x
#define h(x) x + f

int
main ()
{
  if (X(X) != 14)		/* Should expand to "4 + e".  */
    abort ();

  if (X(X(f)) != 108)		/* Should expand to "4 + 4 + f".  */
    abort ();

  if (h(e)(h(e)) != 120)	/* Should expand to "e + e + f".  */
    abort ();

  return 0;
}
