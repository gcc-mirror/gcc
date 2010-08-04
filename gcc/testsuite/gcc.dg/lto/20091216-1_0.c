/* { dg-lto-do run } */

#ifdef __ia64
asm (".globl start_\nstart_: nop 0");
#else
asm (".globl start_; start_: nop");
#endif

int
main ()
{
  return 0;
}
