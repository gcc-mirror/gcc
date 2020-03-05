/* { dg-do compile } */
/* { dg-skip-if "PIC not available for ARC6xx" { arc6xx } } */
/* { dg-options "-mno-sdata -O2 -fpic -fno-builtin" } */

/* Check if we resolve correctly complex PIC addresses.  */

char *foo (unsigned size)
{
  static char buf[32];
  register int i;

  if (size > 31)
    size = 31;

  for (i = 0; i < size; i++)
    {
      buf[i] = ' ';
    }
  buf[size] = '\0';
  return buf;
}

/* { dg-final { scan-assembler "@buf.\[0-9\]\+@pcl-1" } } */
