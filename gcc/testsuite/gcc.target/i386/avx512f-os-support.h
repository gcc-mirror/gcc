/* Check if the OS supports executing AVX512F instructions.  */

static int
avx512f_os_support (void)
{
  unsigned int eax, edx;

  __asm__ ("xgetbv" : "=a" (eax), "=d" (edx) : "c" (0));
  return (eax & 230) == 230;
}
