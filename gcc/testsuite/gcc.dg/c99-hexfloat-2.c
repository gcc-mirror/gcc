/* Test for hex floating point constants: in C99 only.  Preprocessor test.  */
/* Origin: Joseph Myers <jsm28@cam.ac.uk> */
/* { dg-do run } */
/* { dg-options "-std=iso9899:1999 -pedantic-errors" } */

#define f (
#define l )
#define str(x) #x
#define xstr(x) str(x)

/* C90: "0x1p+( 0x1p+)"; C99: "0x1p+f 0x1p+l" */
const char *s = xstr(0x1p+f 0x1p+l);

extern void abort (void);
extern int strcmp (const char *, const char *);

int
main (void)
{
  if (strcmp (s, "0x1p+f 0x1p+l"))
    abort ();
  else
    return 0; /* Correct C99 behaviour.  */
}
