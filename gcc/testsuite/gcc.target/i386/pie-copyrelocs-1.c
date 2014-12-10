/* Check that GOTPCREL isn't used to access glob_a.  */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-require-effective-target pie_copyreloc } */
/* { dg-options "-O2 -fpie" } */

extern int glob_a;

int foo ()
{
  return glob_a;
}

/* glob_a should never be accessed with a GOTPCREL.  */
/* { dg-final { scan-assembler-not "glob_a@GOTPCREL" { target { ! ia32 } } } } */
