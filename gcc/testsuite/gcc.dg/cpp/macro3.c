/* { dg-do run } */
/* { dg-options "-std=c99" } */

/* First two tests sourced from a bug report of Thomas Pornin.
   Varargs test source Jamie Lokier.
   All adapted for the testsuite by Neil Booth, Oct 2000.  */

/* Tests various macro abuse is correctly expanded.  */
static int d = 4;
#define c(x) d
#define d(x) c(2)

#if 0
/* This macro chain above sucks up the whole file once it starts, so
   I've commented it out.  The example is left for idle amusement :-) */
#define a(x) b(
#define b(x) a(
#endif

#define apply(...)   apply2 (__VA_ARGS__)  
#define half(x)      ((x) / 2)
#define apply2(f,x)  f (x)

extern void abort (void);
extern void exit (int);

int main()
{
  /* Expands to c(2) then d.  */
  if (c(c)(c) != 4)
    abort ();

  if (apply (half, 200) != 100)
    abort ();

  exit (0);
}
