/* { dg-do compile } */
/* { dg-options "-O2 -mtune=atom" } */

extern void exit (int);

unsigned char cx = 7;

int
main ()
{
  unsigned char cy;
  unsigned char cz = 1;
  asm ("" : "+q" (cz));

  cy = cx / 6; if (cy != cz) exit (1);
  cy = cx % 6; if (cy != cz) exit (1);

  exit(0);
}

/* { dg-final { scan-assembler-times "divb" 1 } } */
/* { dg-final { scan-assembler-not "divw" } } */
