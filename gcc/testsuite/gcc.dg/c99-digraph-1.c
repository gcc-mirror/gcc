/* Test for recognition of digraphs: should be recognized in C94 and C99
   mode, but not in C90 mode.  Also check correct stringizing.
*/
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#define str(x) xstr(x)
#define xstr(x) #x
#define foo(p, q) str(p %:%: q)

extern void abort (void);
extern int strcmp (const char *, const char *);

int
main (void)
{
  const char *t = foo (1, 2);
  const char *u = str (<:);
  if (strcmp (t, "12") || strcmp (u, "<:"))
    abort ();
  else
    return 0;
}
