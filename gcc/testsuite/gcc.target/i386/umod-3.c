/* { dg-do compile } */
/* { dg-options "-O2 -mtune=atom" } */

extern void abort (void);
extern void exit (int);

unsigned char cx = 7;

int
main ()
{
  unsigned char cy;
  
  cy = cx / 6; if (cy != 1) abort ();
  cy = cx % 6; if (cy != 1) abort ();

  exit(0);
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-not "divw" } } */
