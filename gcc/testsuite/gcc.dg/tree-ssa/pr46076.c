/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error (void);

typedef unsigned char(*Calculable)(void);

static unsigned char one() { return 1; }
static unsigned char two() { return 2; }

static int
print(Calculable calculate)
{
  return calculate() + calculate() + 1;
}

int
main()
{
  /* Make sure we perform indirect inlining of one and two and optimize
     the result to a constant.  */
  if (print(one) != 3)
    link_error ();
  if (print(two) != 5)
    link_error ();
  return 0;
}
