/* PR target/70821 */
/* { dg-do compile } */
/* { dg-options "-Os" } */
/* { dg-additional-options "-march=i686" { target ia32 } } */

void bar (void);

void
foo (int *p)
{
  if (__atomic_sub_fetch (p, 1, __ATOMIC_SEQ_CST))
    bar ();
}

/* { dg-final { scan-assembler "lock;? dec" } } */
/* { dg-final { scan-assembler-not "lock;? xadd" } } */
