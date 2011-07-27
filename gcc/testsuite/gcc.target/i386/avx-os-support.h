/* Check if the OS supports executing AVX instructions.  */

static int
avx_os_support (void)
{
  unsigned int eax, edx;

  __asm__ ("xgetbv" : "=a" (eax), "=d" (edx) : "c" (0));
  return (eax & 6) == 6;
}
