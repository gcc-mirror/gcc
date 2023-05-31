/* { dg-options "isa_rev>=6" } */
/* { dg-skip-if "" { *-*-* } { "-O0" "-Os" } { "" } } */
/* { dg-final { scan-assembler-not "memcpy" } } */
/* { dg-final { scan-assembler-not "lb\t" } } */
/* { dg-final { scan-assembler-not "sb\t" } } */
/* { dg-final { scan-assembler-not "lh\t" } } */
/* { dg-final { scan-assembler-not "sh\t" } } */

char a[4097], b[4097];
#ifdef __mips64
#define MAX_SIZE 128
#else
#define MAX_SIZE 64
#endif

NOCOMPRESSION void
foo ()
{
  __builtin_memcpy(&a[1], &b[1], MAX_SIZE-16);
}
