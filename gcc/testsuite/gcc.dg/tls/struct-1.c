/* This testcase ICEd on IA-32 because the backend was inconsistent whether
   to allow addends for @dtpoff relocs or not.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fpic" } */
/* { dg-require-effective-target tls } */

struct S {
  int s0, s1, s2, s3;
};
static __thread struct S x;
extern void abort (void);
extern void exit (int);

void
foo (struct S *s)
{
  s->s2 = 231;
}

void
bar (void)
{
  if (x.s0 == 231 || x.s2 != 231)
    abort ();
}

int
main ()
{
  foo (&x);
  bar ();
  exit (0);
}
