/* PR target/5755
   This testcase failed because the caller of a function returning struct
   expected the callee to pop up the hidden return structure pointer,
   while callee was actually not poping it up (as the hidden argument
   was passed in register).  */
/* { dg-do run { target i?86-*-* } } */
/* { dg-options "-O2 -fomit-frame-pointer" } */

extern void abort (void);
extern void exit (int);

typedef struct {
   int a1, a2;
} A;

A a;

A __attribute__ ((regparm (2)))
foo (int x)
{
  return a;
}

int __attribute__ ((regparm (2)))
bar (int x)
{
  int r = foo(0).a2;
  return r;
}

int
main ()
{
  int f;
  a.a1 = 530;
  a.a2 = 980;
  f = bar (0);
  if (f != 980)
    abort ();
  exit (0);
}
