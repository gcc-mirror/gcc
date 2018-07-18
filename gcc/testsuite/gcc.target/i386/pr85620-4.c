/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

struct ucontext;

extern int bar (struct ucontext *) __attribute__((__returns_twice__));

static int __attribute__ ((__always_inline__))
foo (struct ucontext *oucp) /* { dg-error "setjmp" } */
{
  return bar (oucp);
}

int
test (struct ucontext *oucp)
{
  return foo (oucp);
}
