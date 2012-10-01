/* Test whether denormal floating point constants in hexadecimal notation
   are parsed correctly.  */
/* { dg-do run } */
/* { dg-options "-std=c99" } */
/* { dg-require-effective-target large_long_double } */

long double d = 0x0.0000003ffffffff00000p-16357L;
long double e = 0x0.0000003ffffffff00000p-16356L;

extern void abort (void);
extern void exit (int);

int
main (void)
{
  if (d != e / 2.0)
    abort ();
  exit (0);
}
