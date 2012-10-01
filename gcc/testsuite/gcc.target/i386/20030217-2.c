/* Test whether denormal floating point constants in hexadecimal notation
   are parsed correctly.  */
/* { dg-do run } */
/* { dg-options "-std=c99" } */

long double d;
long double e;

long double f = 2.2250738585072014E-308L;

extern void abort (void);
extern void exit (int);

int
main (void)
{
   d = 0x0.0000003ffffffff00000p-1048L;
   e = 0x0.0000003ffffffff00000p-1047L;
  if (d != e / 2.0)
    abort ();

  exit (0);
}
