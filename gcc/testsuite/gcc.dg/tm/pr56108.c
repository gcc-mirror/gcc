/* { dg-do compile } */
/* { dg-options "-fgnu-tm -O" } */

int
main()
{
  __transaction_relaxed { __asm__(""); }
  return 0;
}
