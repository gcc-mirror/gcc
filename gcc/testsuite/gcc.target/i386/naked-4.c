/* { dg-do compile } */
/* { dg-options "-O0" } */
/* { dg-additional-options "-mregparm=3" { target ia32 } } */

/* Verify that __attribute__((naked)) produces a naked function 
   that does not allocate stack slots for args.  */
extern void bar (int);

int
__attribute__((naked))
foo (int a, int b, int c)
{
  bar (c);
  asm volatile ("ret" :: "a" (b));
}

/* { dg-final { scan-assembler-not "%\[re\]bp" } } */
