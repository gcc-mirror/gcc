/* { dg-lto-do run } */

asm (".globl start; start: nop");

int
main ()
{
  return 0;
}
