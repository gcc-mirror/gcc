/* { dg-do compile } */
/* { dg-options "-mabi=lp64d -O0 -fno-pic -fno-plt -mexplicit-relocs -mcmodel=extreme" } */
/* { dg-final { scan-assembler "test:.*pcalau12i.*%got_pc_hi20.*\n\taddi\.d.*%got_pc_lo12.*\n\tlu32i\.d.*%got64_pc_lo20.*\n\tlu52i\.d.*%got64_pc_hi12.*\n\tldx\.d" } } */
/* { dg-final { scan-assembler "test1:.*pcalau12i.*%pc_hi20.*\n\taddi\.d.*%pc_lo12.*\n\tlu32i\.d.*%pc64_lo20.*\n\tlu52i\.d.*pc64_hi12.*\n\tadd\.d" } } */
/* { dg-final { scan-assembler "test2:.*pcalau12i.*%pc_hi20.*\n\taddi\.d.*%pc_lo12.*\n\tlu32i\.d.*%pc64_lo20.*\n\tlu52i\.d.*pc64_hi12.*\n\tadd\.d" } } */

extern void g (void);
void
f (void)
{}

static void
l (void)
{}

void
test (void)
{
  g ();
}

void
test1 (void)
{
  f ();
}

void
test2 (void)
{
  l ();
}
