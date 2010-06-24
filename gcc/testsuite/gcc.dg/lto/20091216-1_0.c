/* { dg-lto-do run } */

#ifdef __ia64
asm (".globl start_\n\tstart_: nop 0");
#else
asm (".globl start_\n\tstart_: nop");
#endif

int
main ()
{
  return 0;
}
