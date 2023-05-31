/* { dg-options "isa_rev>=6 -mno-unaligned-access" } */
/* { dg-final { scan-assembler "memcpy" } } */

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
