/* { dg-options "" } */

#pragma GCC target "+nothing+nofp"

void
test (void)
{
  register int q0 asm ("q0"); // { dg-error "not general enough" }
  register int q1 asm ("q1"); // { dg-error "not general enough" }
  asm volatile ("" : "=w" (q0));
  q1 = q0;
  asm volatile ("" :: "w" (q1));
}

void
ok (void)
{
  asm volatile ("" ::: "q0");
}
