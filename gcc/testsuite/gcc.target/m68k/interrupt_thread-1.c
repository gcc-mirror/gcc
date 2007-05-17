/* { dg-do compile } */
/* { dg-options "-mcpu=fidoa -O2 -fomit-frame-pointer" } */

/* Check that interrupt_thread attribute works.  */

#ifdef __mfido__
extern void foo (void) __attribute__ ((interrupt_thread));

int a, b, c, d;

void bar (void);

void
foo (void)
{
  int w, x, y, z;

  w = a;
  x = b;
  y = c;
  z = d;

  bar ();

  a = w;
  b = x;
  c = y;
  d = z;
}
#else
/* If the current mutilib is, say, -mcpu=5485, the compiler gets
   -mcpu=fidoa -mcpu=5485, where -mcpu=fidoa is overridden.  In that
   case, we just print out "sleep" in the assembly file and pretend
   that everything is all right.  */
asm ("sleep");
#endif

/* "sleep" should be generated in place of "rts".  */
/* { dg-final { scan-assembler-times "sleep" 1 } } */
/* { dg-final { scan-assembler-times "rts" 0 } } */

/* There should be no stack adjustment.  */
/* { dg-final { scan-assembler-times "sp" 0 } } */
