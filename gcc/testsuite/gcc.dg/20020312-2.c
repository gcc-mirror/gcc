/* PR 5878. 

   We ICEd in verify_local_live_at_start because we incorrectly forced
   the PIC register live between BAR and BAZ.  We did this because the
   definition of PIC_OFFSET_TABLE_REGNUM was incorrectly not INVALID_REGNUM
   when !flag_pic for most targets.  */

/* { dg-do run } */
/* { dg-options "-O -fno-pic" } */

#if #cpu(a29k)
/* No pic register.  */
#elif defined(__alpha__)
/* PIC register is $29, but is used even without -fpic.  */
#elif defined(__arc__)
# define PIC_REG  "26"
#elif defined(__arm__)
# define PIC_REG  "9"
#elif defined(AVR)
/* No pic register.  */
#elif defined(__clipper__)
/* No pic register.  */
#elif defined(__convex__)
/* No pic register.  */
#elif defined(__cris__)
# define PIC_REG  "0"
#elif defined(__D30V__)
/* No pic register.  */
#elif defined(__dsp1600__)
/* No pic register.  */
#elif defined(__elxsi__)
/* No pic register.  */
#elif defined(__fr30__)
/* No pic register.  */
#elif defined(__H8300__) || defined(__H8300H__) || defined(__H8300S__)
/* No pic register.  */
#elif #cpu(i370)
/* No pic register.  */
#elif defined(__i386__)
# define PIC_REG  "ebx"
#elif defined(__i860__)
/* No pic register.  */
#elif defined(__i960__)
/* No pic register.  */
#elif defined(__ia64__)
/* PIC register is r1, but is used even without -fpic.  */
#elif defined(__M32R__)
/* No pic register.  */
#elif defined(__m68k__)
# define PIC_REG  "a5"
#elif defined(__m88k__)
# define PIC_REG  "25"
#elif defined(__mc68hc1x__)
/* No pic register.  */
#elif defined(__mcore__)
/* No pic register.  */
#elif defined(__mips__)
/* PIC register is $28, but is used even without -fpic.  */
#elif defined(__MMIX__)
/* No pic register.  */
#elif defined(__mn10200__)
/* No pic register.  */
#elif defined(__mn10300__)
/* No pic register.  */
#elif #cpu(ns32k)
/* No pic register.  */
#elif defined(__parisc__)
/* PIC register is %r27 or %r19, but is used even without -fpic.  */
#elif defined(__pdp11__)
/* No pic register.  */
#elif defined(__pj__)
/* No pic register.  */
#elif defined(__powerpc__)
# ifdef __darwin__
#  define PIC_REG  "31"
# else
#  define PIC_REG  "30"
# endif
#elif defined(__ibm032__) /* aka romp */
/* No pic register.  */
#elif defined(__s390__)
# define PIC_REG  "12"
#elif defined(__sparc__)
# define PIC_REG  "l7"
#elif defined(__v850)
/* No pic register.  */
#elif defined(__vax__)
/* No pic register.  */
#elif defined(__we32000__)
/* No pic register.  */
#elif defined(__xstormy16__)
/* No pic register.  */
#elif defined(__XTENSA__)
/* No pic register.  */
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
     register is in a register window, of course.  */
#ifndef __sparc__
  if (reg)
    abort ();
#endif

  reg = old_reg;
  return 0;
}
