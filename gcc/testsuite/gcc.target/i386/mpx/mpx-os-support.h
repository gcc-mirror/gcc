/* Check if the OS supports executing MPX instructions.  */

#define XCR_XFEATURE_ENABLED_MASK	0x0

#define XSTATE_BNDREGS	0x8

static int
mpx_os_support (void)
{
  unsigned int eax, edx;
  unsigned int ecx = XCR_XFEATURE_ENABLED_MASK;

  __asm__ ("xgetbv" : "=a" (eax), "=d" (edx) : "c" (ecx));

  return (eax & XSTATE_BNDREGS) != 0;
}
