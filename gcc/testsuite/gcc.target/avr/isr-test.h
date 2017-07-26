#ifndef ISR_TEST_H
#define ISR_TEST_H

#include <string.h>

#define ISR(N,...)                                                      \
__attribute__ ((used, externally_visible , ## __VA_ARGS__))             \
    void __vector_##N (void);                                           \
    void __vector_##N (void)

#define SFR(ADDR) (*(unsigned char volatile*) (__AVR_SFR_OFFSET__ + (ADDR)))
#define CORE_SFRS SFR (0x38)
#define SREG      SFR (0x3F)
#define SPL       SFR (0x3D)
#define EIND      SFR (0x3C)
#define RAMPZ     SFR (0x3B)
#define RAMPY     SFR (0x3A)
#define RAMPX     SFR (0x39)
#define RAMPD     SFR (0x38)

#ifdef __AVR_HAVE_JMP_CALL__
#define VEC_SIZE 4
#else
#define VEC_SIZE 2
#endif

#ifdef __AVR_TINY__
#define FIRST_REG 16
#else
#define FIRST_REG 0
#endif

#define CR "\n\t"

typedef struct
{
  unsigned char sfrs[8];
  unsigned char gprs[32 - FIRST_REG];
} regs_t;

regs_t reginfo1, reginfo2;

__attribute__((noinline))
static void clear_reginfo (void)
{
  memset (reginfo1.sfrs, 0, sizeof (reginfo1.sfrs));
  memset (reginfo2.sfrs, 0, sizeof (reginfo2.sfrs));
}

__attribute__((noinline))
static void compare_reginfo (unsigned long gpr_ignore)
{
  signed char regno;
  const unsigned char *preg1 = &reginfo1.gprs[0];
  const unsigned char *preg2 = &reginfo2.gprs[0];

  if (memcmp (&reginfo1, &reginfo2, 8))
    __builtin_abort();

  gpr_ignore >>= FIRST_REG;

    for (regno = FIRST_REG; regno < 32;
       regno++, preg1++, preg2++, gpr_ignore >>= 1)
    {
      if (gpr_ignore & 1)
        continue;

      if (*preg1 != *preg2)
        {
          static signed char volatile failed_regno;
          failed_regno = regno;
          __builtin_abort();
        }
    }
}

/* STore GPR */
#define ST(regno,M)                                     \
  CR "sts %[" #M "]+8-%[first]+" #regno ", r" #regno

/* STore SFR */
#define ST_SFR(sfr, n_sfr, M)                   \
  CR "in __tmp_reg__,%i[s_" #sfr "]"            \
  CR "sts %[" #M "]+" #n_sfr ", __tmp_reg__"

/* Named asm OPerand for SFR */
#define OP_SFR(sfr)                             \
  , [s_ ## sfr] "n" (&(sfr))

/* Write funny value to SFR */
#define XX_SFR(sfr)                             \
  CR "dec r31 $ out %i[s_" #sfr "], r31"

/* Write 0 to SFR */
#define OO_SFR(sfr)                             \
  CR "out %i[s_" #sfr "], __zero_reg__"

/* Macros for SREG */
#define ST_SREG(M) ST_SFR (SREG,0,M)
#define OP_SREG    OP_SFR (SREG)
#define XX_SREG    XX_SFR (SREG)

/* Macros for EIND */
#if defined __AVR_HAVE_EIJMP_EICALL__
#define ST_EIND(M) ST_SFR (EIND,1,M)
#define OP_EIND    OP_SFR (EIND)
#else
#define ST_EIND(M) /* empty */
#define OP_EIND    /* empty */
#endif

/* Macros for RAMPX */
#if defined (__AVR_HAVE_RAMPX__)
#define ST_RAMPX(M) ST_SFR (RAMPX,2,M)
#define OP_RAMPX    OP_SFR (RAMPX)
#define XX_RAMPX    XX_SFR (RAMPX)
#define OO_RAMPX    OO_SFR (RAMPX)
#else
#define ST_RAMPX(M) /* empty */
#define OP_RAMPX    /* empty */
#define XX_RAMPX    /* empty */
#define OO_RAMPX    /* empty */
#endif

/* Macros for RAMPY */
#if defined (__AVR_HAVE_RAMPY__)
#define ST_RAMPY(M) ST_SFR (RAMPY,3,M)
#define OP_RAMPY    OP_SFR (RAMPY)
#define XX_RAMPY    XX_SFR (RAMPY)
#define OO_RAMPY    OO_SFR (RAMPY)
#else
#define ST_RAMPY(M) /* empty */
#define OP_RAMPY    /* empty */
#define XX_RAMPY    /* empty */
#define OO_RAMPY    /* empty */
#endif

/* Macros for RAMPZ */
#if defined (__AVR_HAVE_RAMPZ__)
#define ST_RAMPZ(M) ST_SFR (RAMPZ,4,M)
#define OP_RAMPZ    OP_SFR (RAMPZ)
#define XX_RAMPZ    XX_SFR (RAMPZ)
#define OO_RAMPZ    OO_SFR (RAMPZ)
#else
#define ST_RAMPZ(M) /* empty */
#define OP_RAMPZ    /* empty */
#define XX_RAMPZ    /* empty */
#define OO_RAMPZ    /* empty */
#endif

/* Macros for RAMPD */
#if defined (__AVR_HAVE_RAMPD__)
#define ST_RAMPD(M) ST_SFR (RAMPD,5,M)
#define OP_RAMPD    OP_SFR (RAMPD)
#else
#define ST_RAMPD(M) /* empty */
#define OP_RAMPD    /* empty */
#endif

/* Macros for all GPRs */
#if defined __AVR_TINY__
#define ST_REGS_LO(M) /* empty */
#else
#define ST_REGS_LO(M)                           \
  ST(0,M)   ST(1,M)   ST(2,M)   ST(3,M)         \
  ST(4,M)   ST(5,M)   ST(6,M)   ST(7,M)         \
  ST(8,M)   ST(9,M)   ST(10,M)  ST(11,M)        \
  ST(12,M)  ST(13,M)  ST(14,M)  ST(15,M)
#endif /* AVR_TINY */

#define ST_REGS_HI(M)                           \
  ST(16,M)    ST(17,M)    ST(18,M)    ST(19,M)  \
  ST(20,M)    ST(21,M)    ST(22,M)    ST(23,M)  \
  ST(24,M)    ST(25,M)    ST(26,M)    ST(27,M)  \
  ST(28,M)    ST(29,M)    ST(30,M)    ST(31,M)

__attribute__((unused,naked,noinline,noclone))
static void host_store1 (void)
{
  __asm __volatile__
  ("nop"
   CR ".global do_stores_before"
   CR ".type   do_stores_before,@function"
   CR "do_stores_before:"
   /* Funny values to some SFRs */
   CR "ldi r31, 1 + 'Z'"
   XX_RAMPZ
   XX_RAMPY
   XX_RAMPX
   CR "dec __zero_reg__"
   CR "clr r31"
   XX_SREG
   /* Must set I-flag due to RETI of ISR */
   CR "sei"
   /* Store core regs before ISR */
   ST_RAMPX (mem1)
   ST_RAMPY (mem1)
   ST_RAMPZ (mem1)
   ST_RAMPD (mem1)
   ST_EIND  (mem1)
   ST_SREG  (mem1)
   CR "ldi r31, 0xaa"
   CR "mov __tmp_reg__, r31"
   CR "ldi r31, 31"
   ST_REGS_LO (mem1)
   ST_REGS_HI (mem1)
   CR "ret"
   : /* No outputs */
   : [mem1] "i" (&reginfo1), [first] "n" (FIRST_REG)
   OP_RAMPX
   OP_RAMPY
   OP_RAMPZ
   OP_RAMPD
   OP_EIND
   OP_SREG
   : "memory", "r31");
}

__attribute__((unused,naked,noinline,noclone))
static void host_store2 (void)
{
  __asm __volatile__
  ("nop"
   CR ".global do_stores_after"
   CR ".type   do_stores_after,@function"
   CR "do_stores_after:"
   /* Store core regs after ISR */
   ST_REGS_LO (mem2)
   ST_REGS_HI (mem2)
   ST_RAMPX (mem2)
   ST_RAMPY (mem2)
   ST_RAMPZ (mem2)
   ST_RAMPD (mem2)
   ST_EIND  (mem2)
   ST_SREG  (mem2)
   /* Undo funny values */
   CR "clr __zero_reg__"
   OO_RAMPX
   OO_RAMPY
   OO_RAMPZ
   CR "ret"
   : /* No outputs */
   : [mem2] "i" (&reginfo2), [first] "n" (FIRST_REG)
   OP_RAMPX
   OP_RAMPY
   OP_RAMPZ
   OP_RAMPD
   OP_EIND
   OP_SREG
   : "memory");
}

#define MK_CALL_ISR(vecno)                      \
  __asm __volatile__                            \
  (/* Funny values to some SFRs */              \
   /* Must set I-flag due to RETI of ISR */     \
   /* Store core regs before ISR */             \
   CR "%~call do_stores_before"                 \
   /* Execute ISR */                            \
   CR "%~call __vectors + %[vect]"              \
   /* Store core regs after ISR */              \
   /* Undo funny values */                      \
   CR "%~call do_stores_after"                  \
   : /* No outputs */                           \
   : [vect] "i" (VEC_SIZE * (vecno))            \
   , "i" (host_store1)                          \
   , "i" (host_store2)                          \
   : "memory", "r31")


#define MK_RUN_ISR(N, IGMSK)                    \
                                                \
__attribute__((noinline,noclone))               \
void run_isr_ ## N (void)                       \
{                                               \
  clear_reginfo();                              \
  MK_CALL_ISR (N);                              \
  compare_reginfo (IGMSK);                      \
}

#endif /* ISR_TEST_H */

