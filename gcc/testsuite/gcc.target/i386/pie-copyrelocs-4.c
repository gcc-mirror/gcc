/* Check that GOTPCREL is used to access glob_a.  */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-require-effective-target pie_copyreloc } */
/* { dg-options "-O2 -fpie" } */

extern int glob_a  __attribute__((weak));

int foo ()
{
  if (&glob_a != 0)
    return glob_a;
  else
    return 0;
}

/* weak glob_a should be accessed with a GOTPCREL.  */
/* { dg-final { scan-assembler "glob_a@GOTPCREL" { target { ! ia32 } } } } */
