/* PR target/128960 */
/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=global" } */
/* { dg-require-effective-target fstack_protector } */

void sink (char *);

__attribute__((target ("bmi2")))
void
with_bmi2 (void)
{
  char buf[16];
  sink (buf);
}

void
without_bmi2 (void)
{
  char buf[16];
  sink (buf);
}

/* Both functions must use the global guard, not the TLS one.  */
/* { dg-final { scan-assembler-not "%\[fg\]s:" } } */
/* { dg-final { scan-assembler-times "__stack_chk_guard" 4 } } */
