/* { dg-do run } */
/* { dg-options "-std=c99" } */

/* First two tests sourced from a bug report of Thomas Pornin.
   Varargs test source Jamie Lokier.
   All adapted for the testsuite by Neil Booth, Oct 2000.  */

int c(int x)
{
  return x;
}

int a(int x)
{
  return x;
}

/* Tests various macro abuse is correctly expanded.  */
#define c(x) d
#define d(x) c(2)

/* Every GCC <= 2.96 appears to fail this.  */
#define a(x) b(
#define b(x) a(

#define apply(...)   apply2 (__VA_ARGS__)  
#define half(x)      ((x) / 2)
#define apply2(f,x)  f (x)

extern void abort (void);
extern void exit (int);

int main()
{
  /* Expands to c(2).  */
  if (c(c)(c) != 2)
    abort ();

  /* Expands to a(2).  */
  if (a(a)x)2) != 2)
    abort ();

  if (apply (half, 200) != 100)
    abort ();

  exit (0);
}
