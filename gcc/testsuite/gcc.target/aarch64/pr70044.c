/* { dg-do link } */
/* { dg-require-effective-target lto } */
/* { dg-options "-flto -O --save-temps -fno-omit-frame-pointer" } */

extern int atoi (const char *);

int
main (int argc, char **argv)
{
  return atoi (argv[0]) + 1;
}

/* Check that the frame pointer really is created.  */
/* { dg-final { scan-lto-assembler "(mov|add)	x29, sp" } } */
