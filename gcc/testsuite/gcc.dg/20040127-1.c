/* PR target/10904 */
/* Origin: <kminola@eng.umd.edu> */

/* Verify that the register allocator correctly aligns
   floating-point registers on SPARC64.  */

/* { dg-do assemble } */
/* { dg-options "-std=gnu17 -O2" } */

extern int foo1();
extern int foo2();

void foo(int n, int b)
{
  int i, a;

  foo1();

  a = (long)(b * ((double) 0.1));

  for (i=0; i < n; i++) {
    foo2(a);
  }
}
