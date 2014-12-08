/* Check that PLT is used to access glob_a.  */
/* { dg-do compile { target *-*-linux* } } */
/* { dg-require-effective-target pie_copyreloc } */
/* { dg-options "-O2 -fpie" } */

extern int glob_a (void);

int foo ()
{
  return glob_a ();
}

/* glob_a should be accessed with a PLT.  */
/* { dg-final { scan-assembler "glob_a@PLT" { target { ! ia32 } } } } */
