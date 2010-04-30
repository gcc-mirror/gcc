/* { dg-lto-do run } */

asm (".globl start_; start_: nop");

int
main ()
{
  return 0;
}
