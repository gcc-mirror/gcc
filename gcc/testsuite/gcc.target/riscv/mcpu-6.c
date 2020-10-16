/* { dg-do compile } */
/* Verify -mtune has higher priority than -mcpu for pipeline model .  */
/* { dg-options "-mcpu=sifive-u74 -mtune=rocket -fdump-rtl-sched2-details -O3 -march=rv32i -mabi=ilp32" } */
/* { dg-final { scan-rtl-dump "simple_return\[ \]+:alu" "sched2" } } */

int main()
{
  return 0;
}

