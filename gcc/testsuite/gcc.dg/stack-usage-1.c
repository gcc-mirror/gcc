/* { dg-do compile } */
/* { dg-options "-fstack-usage -fno-stack-protector" } */
/* nvptx doesn't have a reg allocator, and hence no stack usage data.  */
/* { dg-skip-if "" { nvptx-*-* } } */

/* This is aimed at testing basic support for -fstack-usage in the back-ends.
   See the SPARC back-end for example (grep flag_stack_usage_info in sparc.c).
   Once it is implemented, adjust SIZE below so that the stack usage for the
   function FOO is reported as 256 or 264 in the stack usage (.su) file.
   Then check that this is the actual stack usage in the assembly file.  */

#if defined(__aarch64__)
#  define SIZE 256 /* No frame pointer for leaf functions (default) */
#elif defined(__arc__)
#  define SIZE (256-4)
#elif defined(__i386__)
#  define SIZE 248
#elif defined(__x86_64__)
#  ifndef _WIN64
#    define SIZE 356
#  else
#    define SIZE (256 - 24)
#  endif
#elif defined (__sparc__)
#  if defined (__arch64__)
#    define SIZE 76
#  else
#    define SIZE 160
#  endif
#elif defined(__hppa__)
#  define SIZE 192
#elif defined (__alpha__)
#  define SIZE 240
#elif defined (__ia64__)
#  define SIZE 272
#elif defined(__mips__)
#  if defined (__mips_abicalls) \
      || (defined _MIPS_SIM && (_MIPS_SIM ==_ABIN32 || _MIPS_SIM==_ABI64))
#    define SIZE 240
#  else
#    define SIZE 248
#  endif
#elif defined (__nds32__)
#  define SIZE 248 /* 256 - 8 bytes, only $fp and padding bytes are saved in
                      the register save area under O0 optimization level.  */
#elif defined (__powerpc64__) || defined (__ppc64__) || defined (__POWERPC64__) \
      || defined (__PPC64__)
#  if _CALL_ELF == 2
#     define SIZE 208
#  else
#     define SIZE 180
#  endif
#elif defined (__powerpc__) || defined (__PPC__) || defined (__ppc__) \
      || defined (__POWERPC__) || defined (PPC) || defined (_IBMR2)
#  if defined (__ALTIVEC__)
#    if defined (__APPLE__)
#      define SIZE 204
#    else
#      define SIZE 220
#    endif
#  elif defined (_AIX)
#    define SIZE 208
#  else
#    define SIZE 240
#  endif
#elif defined (__riscv)
#  if defined (__riscv_32e)
#    define SIZE 252
#  else
#    define SIZE 240
#  endif
#elif defined (__AVR__)
#if defined (__AVR_3_BYTE_PC__ )
#  define SIZE 251 /* 256 - 2 bytes for Y - 3 bytes for return address */
#else
#  define SIZE 252 /* 256 - 2 bytes for Y - 2 bytes for return address */
#endif
#elif defined (__s390x__)
#  define SIZE 96  /* 256 - 160 bytes for register save area */
#elif defined (__s390__)
#  define SIZE 160 /* 256 -  96 bytes for register save area */
#elif defined (__epiphany__)
#  define SIZE (256 - __EPIPHANY_STACK_OFFSET__)
#elif defined (__RL78__)
#  define SIZE 254
#elif defined (__sh__)
#  define SIZE 252
#elif defined (__frv__)
#  define SIZE 248
#elif defined (xstormy16)
#  define SIZE 254
#elif defined (__PRU__)
#  define SIZE 252
#elif defined (__v850__)
#define SIZE 260
#elif defined (__mn10300__)
#define SIZE 252
#elif defined (__H8300SX__) || defined (__H8300S__) || defined (__H8300H__) || defined (__H8300__) 
#define SIZE 252
#elif defined (__M32R__)
#define SIZE 252
#elif defined (__csky__)
#  define SIZE 252
#elif defined (__CRIS__)
#  define SIZE 252
#elif defined (__loongarch_lp64)
#  define SIZE 240   /* 256 - 8 bytes for $fp, and 8 bytes for a temp value */
#else
#  define SIZE 256
#endif

int foo (void)
{
  char arr[SIZE];
  arr[0] = 1;
  return 0;
}

/* { dg-final { scan-stack-usage "foo\t\(256|264\)\tstatic" } } */
/* { dg-final { cleanup-stack-usage } } */
