/* Test always_inline attribute, which forces inlining of functions
   even at no optimization.  */
/* Origin: Aldy Hernandez <aldyh@redhat.com>.  */
/* { dg-do compile } */
/* { dg-options "-O0" } */

static inline int sabrina (void) __attribute__((always_inline));

static inline int sabrina (void)
{
  return 13;
}

int bar (void)
{
  return sabrina () + 68;
}

/* { dg-final { scan-assembler-not "sabrina" } } */
