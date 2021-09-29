/* { dg-do compile } */
/* { dg-options "-O2 -mstrict-align" } */

struct s { char x[255]; };
void foo (struct s *);
void bar (void) { struct s s1 = {}; foo (&s1); }

/* memset (s1 = {}, sizeof = 255) should be expanded out
   such that there are no overlap stores when -mstrict-align
   is in use.
   so 7 pairs of 16 bytes stores (224 bytes).
   1 16 byte stores
   1 8 byte store
   1 4 byte store
   1 2 byte store
   1 1 byte store
   */

/* { dg-final { scan-assembler-times "stp\tq" 7 } } */
/* { dg-final { scan-assembler-times "str\tq" 1 } } */
/* { dg-final { scan-assembler-times "str\txzr" 1 } } */
/* { dg-final { scan-assembler-times "str\twzr" 1 } } */
/* { dg-final { scan-assembler-times "strh\twzr" 1 } } */
/* { dg-final { scan-assembler-times "strb\twzr" 1 } } */

/* Also one store pair for the frame-pointer and the LR. */
/* { dg-final { scan-assembler-times "stp\tx" 1 } } */

