/* { dg-options "(-mmicromips)" } */
/* { dg-do assemble } */

register unsigned int global asm ("$16");

extern void exit (int) __attribute__((noreturn));

MICROMIPS void
test_sb (unsigned char *ptr, void (*f) (void))
{
  ptr[0] = global;
  f ();
  exit (0);
}

MICROMIPS void
test_sh (unsigned short *ptr, void (*f) (void))
{
  ptr[0] = global;
  f ();
  exit (0);
}

MICROMIPS void
test_sw (unsigned int *ptr, void (*f) (void))
{
  ptr[0] = global;
  f ();
  exit (0);
}
