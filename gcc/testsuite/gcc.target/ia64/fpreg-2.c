/* Test __fpreg ABI.  */
/* Origin: Joseph Myers <joseph@codesourcery.com> */
/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-final { scan-assembler "ldf.fill" } } */
/* { dg-final { scan-assembler "stf.spill" } } */

__fpreg x;

void f (void);

void
g (void)
{
  __fpreg b = x;
  f ();
  x = b;
}

char t1[(sizeof (__fpreg) == sizeof (__float80) ? 1 : -1)];
char t2[(__alignof (__fpreg) == __alignof (__float80) ? 1 : -1)];
