/* Test whether denormal floating point constants in hexadecimal notation
   are parsed correctly.  */
/* { dg-do run { target i?86-*-linux* x86_64-*-* } } */
/* { dg-options "-std=c99" } */

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
