/* { dg-do run { target powerpc*-*-darwin* } } */

/* This check was originally in test vmx/varargs-4.c.  It does not
   match the expected behavior according to the PowerPC-64 ELF ABI.  */

#include <altivec.h>

extern void abort (void);
extern void exit (int);

typedef struct n_a
{
  signed char m1;
  short m2;
  int m3;
  double m4;
  vector float m5;
}
n_a;

typedef struct n_a_x
{
  n_a b;
  char a;
}
n_a_x;

int
main ()
{
    if (sizeof (n_a_x) - sizeof (n_a) != sizeof (n_a))
        abort ();
    exit (0);
}
