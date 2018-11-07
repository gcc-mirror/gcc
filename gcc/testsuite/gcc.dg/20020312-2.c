/* PR 5878. 

   We ICEd in verify_local_live_at_start because we incorrectly forced
   the PIC register live between BAR and BAZ.  We did this because the
   definition of PIC_OFFSET_TABLE_REGNUM was incorrectly not INVALID_REGNUM
   when !flag_pic for most targets.  */

/* { dg-do run } */
/* { dg-options "-O -fno-pic" } */
/* { dg-additional-options "-no-pie" { target pie_enabled } } */
/* { dg-require-effective-target nonlocal_goto } */

extern void abort (void);

#if defined(__alpha__)
/* PIC register is $29, but is used even without -fpic.  */
#elif defined(__arc__)
# define PIC_REG  "26"
#elif defined(__arm__)
# define PIC_REG  "9"
#elif defined(AVR)
/* No pic register.  */
#elif defined(__cris__)
# define PIC_REG  "0"
#elif defined(__epiphany__)
#define PIC_REG "r28"
#elif defined(__fr30__)
/* No pic register.  */
#elif defined(__H8300__) || defined(__H8300H__) || defined(__H8300S__)
/* No pic register.  */
#elif defined(_IBMR2)
/* No pic register.  */
#elif defined(__i386__)
# define PIC_REG  "ebx"
#elif defined(__ia64__)
/* PIC register is r1, but is used even without -fpic.  */
#elif defined(__lm32__)
/* No pic register.  */
#elif defined(__M32R__)
/* No pic register.  */
#elif defined(__m68k__)
# define PIC_REG  "a5"
#elif defined(__mc68hc1x__)
/* No pic register.  */
#elif defined(__mcore__)
/* No pic register.  */
#elif defined(__MICROBLAZE__)
# define PIC_REG "r20"
#elif defined(__mips__)
/* PIC register is $28, but is used even without -fpic.  */
#elif defined(__MMIX__)
/* No pic register.  */
#elif defined(__mn10300__)
/* No pic register.  */
#elif defined(__moxie__)
/* No pic register.  */
#elif defined(__nds32__)
/* No pic register.  */
#elif defined(__nios2__)
/* No pic register.  */
#elif defined(__hppa__)
/* PIC register is %r27 or %r19, but is used even without -fpic.  */
#elif defined(__pdp11__)
/* No pic register.  */
#elif defined(__powerpc__) || defined(__PPC__) || defined(__POWERPC__) || defined (__ppc)
# ifdef __MACH__
#  define PIC_REG  "31"
# else
#  define PIC_REG  "30"
# endif
#elif defined(__riscv)
/* No pic register.  */
#elif defined(__RX__)
/* No pic register.  */
#elif defined(__s390__)
# define PIC_REG  "12"
#elif defined(__sparc__)
# define PIC_REG  "l7"
#elif defined(__tile__)
# define PIC_REG "r51"
#elif defined(__TMS320C6X__)
# define PIC_REG "B14"
#elif defined(__v850)
/* No pic register.  */
#elif defined(__vax__)
/* No pic register.  */
#elif defined(__VISIUM__)
/* No pic register.  */
#elif defined(__xstormy16__)
/* No pic register.  */
#elif defined(__XTENSA__)
/* No pic register.  */
#elif defined(__sh__)
# define PIC_REG  "r12"
#elif defined(__x86_64__)
/* No pic register.  */
#elif defined(__m32c__)
/* No pic register.  */
#elif defined(__SPU__)
#  define PIC_REG  "126"
#elif defined (__frv__)
# ifdef __FRV_FDPIC__
#  define PIC_REG "gr15"
# else
#  define PIC_REG "gr17"
#endif
#elif defined (__aarch64__)
/* No pic register -- yet.  */
#elif defined(__RL78__)
/* No pic register.  */
#elif defined(__MSP430__)
/* No pic register.  */
#elif defined (__nvptx__)
/* No pic register.  */
#elif defined (__csky__)
/* Pic register is r28, but some cores only have r0-r15.  */
# if defined (__CK807__) || defined (__CK810__)
#   define PIC_REG  "r28"
# endif
#else
# error "Modify the test for your target."
#endif

#if defined PIC_REG && !defined __PIC__ && !defined __pic__
register void *reg __asm__(PIC_REG);
#else
/* We really need a global register variable set to the PIC register
   to expose the bug.  Oh well, let the test case not fail.  */
static void *reg;
#endif

void * __attribute__((noinline))
dummy (void *x)
{
  return x;
}

void
f (void)
{
  goto *dummy (&&bar);
  for (;;)
    {
    foo:
      reg = (void *) 1;
      if (!reg)
        goto baz;
      reg = &&foo;
    }

 bar:
 baz:
  reg = 0;
}

int
main()
{
  void *old_reg = reg;
  reg = (void *) 1;

  f ();

  /* Additionally test that the prologue/epilogue properly does *not*
     save and restore global registers.  Not possible when the PIC
     register is in a register window, of course.  On Darwin, you can't
     call library routines from non-PIC code.  */
#if !defined (__sparc__) && !defined(__MACH__)
  if (reg)
    abort ();
#endif

  reg = old_reg;
  return 0;
}
