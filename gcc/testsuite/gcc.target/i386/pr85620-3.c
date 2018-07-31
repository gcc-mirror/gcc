/* { dg-do compile } */
/* { dg-options "-O2 -Wno-attributes" } */

struct ucontext;

extern int bar (struct ucontext *) __attribute__((__indirect_return__));

static int __attribute__ ((__always_inline__))
foo (struct ucontext *oucp)
{
  return bar (oucp);
}

int
test (struct ucontext *oucp)
{
  return foo (oucp);
}
