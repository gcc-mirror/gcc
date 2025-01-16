typedef __UINT8_TYPE__   u8;
typedef __UINT16_TYPE__  u16;
__extension__ typedef __uint24         u24;
typedef __UINT32_TYPE__  u32;
typedef __UINT64_TYPE__  u64;

__attribute__((__used__))
u8 gprs[32];

// USE_VALUE = 1: Copy constant to value[] and pass that to test_gprs().
// USE_VALUE = 0: Pass Lval label from .macro run_test_gprs to test_gprs().
#ifndef USE_VALUE
#error define USE_VALUE to 0 or 1
#endif

#if USE_VALUE
__attribute__((__used__))
u8 value[8];
#endif

#ifdef __AVR_HAVE_JMP_CALL__
#define XCALL "call"
#else
#define XCALL "rcall"
#endif

#define GPRS_16_29 "16,17,18,19,20,21,22,23,24,25,26,27,28,29"

#ifdef __AVR_TINY__
#define FIRST_GPR 16
#define GPRs_29 GPRS_16_29
#else
#define FIRST_GPR 0
#define GPRs_29 "0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15," GPRS_16_29
#endif

#if __AVR_RODATA_IN_RAM__
#define PGM __attribute__((progmem))
#else
#define PGM
#endif

// Save GPRs to gprs[].
__attribute__((naked, used))
void save_gprs (void)
{
    __asm (
#ifdef __AVR_TINY__
           "push r31"              "\n\t"
           "push r30"              "\n\t"
           "ldi r30, lo8(gprs+%0)" "\n\t"
           "ldi r31, hi8(gprs+%0)" "\n\t"
           ".irp n," GPRs_29       "\n\t"
           "    st z+, \\n"        "\n\t"
           ".endr"                 "\n\t"
           "pop r16 $ st z+, r16"  "\n\t"
           "pop r16 $ st z+, r16"  "\n\t"
#else
           "sts gprs+31, r31"      "\n\t"
           "sts gprs+30, r30"      "\n\t"
           "ldi r30, lo8(gprs+%0)" "\n\t"
           "ldi r31, hi8(gprs+%0)" "\n\t"
           ".irp n," GPRs_29       "\n\t"
           "    st z+, \\n"        "\n\t"
           ".endr"                 "\n\t"
#endif
           "ret"
           :: "n" (FIRST_GPR));
}

// Restore GPRs from gprs[].
__attribute__((naked, used))
void restore_gprs (void)
{
    __asm ("ldi r30, lo8(gprs+%0)"   "\n\t"
           "ldi r31, hi8(gprs+%0)"   "\n\t"
           ".irp n," GPRs_29         "\n\t"
           "    ld \\n, z+"          "\n\t"
           ".endr"                   "\n\t"
#ifdef __AVR_TINY__
           "push r29"                "\n\t"
           "ld   r29, z+"            "\n\t"
           "push r29"                "\n\t"
           "ld   r31, z"             "\n\t"
           "pop  r30"                "\n\t"
           "pop  r29"                "\n\t"
#else
           "ld  r30, z"              "\n\t"
           "lds r31, gprs+31"        "\n\t"
#endif
           "ret"
           :: "n" (FIRST_GPR));
}

// Write N-byte const value VAL:  *Z++ = VAL.
__asm (".macro write_value n, val"                            "\n\t"
       "    ldi r24, lo8(\\val)"                              "\n\t"
       "    st  z+, r24"                                      "\n\t"
       "    .if \\n > 1"                                      "\n\t"
       "        write_value \"(\\n - 1)\", \"(\\val >> 8)\""  "\n\t"
       "    .endif"                                           "\n\t"
       ".endm");

// 1) Save all gprs
// 2) Call test_gprs (n, regno, Lval, line)
// 3) Restore all gprs
__asm (".macro run_test_gprs n, regno, val, Lval, line"     "\n\t"
       "TestForLine\\line\\().L\\@:"                        "\n\t"
       "    " XCALL " save_gprs"                            "\n\t"
# if USE_VALUE
       // Write VAL to value[]
       "    ldi r30, lo8(\\Lval)"                           "\n\t"
       "    ldi r31, hi8(\\Lval)"                           "\n\t"
       "    write_value \\n, \\val"                         "\n\t"
#endif
       // Call test_gprs (u8 n, u8 regno, const void *Lval, int line)
       "    ldi r24, \\n"                                   "\n\t"
       "    ldi r22, \\regno"                               "\n\t"
       "    ldi r21, hi8(\\Lval)"                           "\n\t"
       "    ldi r20, lo8(\\Lval)"                           "\n\t"
       "    ldi r19, hi8(\\line)"                           "\n\t"
       "    ldi r18, lo8(\\line)"                           "\n\t"
       "    " XCALL " test_gprs"                            "\n\t"
       "    " XCALL " restore_gprs"                         "\n\t"
       "DoneTestLine\\line\\().L\\@:"                       "\n\t"
       ".endm");

// Test if reg[REG] ... reg[REG + BITS/8 - 1] are holding VAL.
#define rtest(BITS, REG, VAL)                                           \
  do {                                                                  \
    PGM static const u##BITS Lval = VAL;                                \
    register u##BITS r##REG __asm (#REG) = VAL;                         \
    __asm ("run_test_gprs %[size], %[reg], %[val], %[Lval], %[line] "   \
           "; u" #BITS " r" #REG "=" #VAL ", line %[line];"             \
           ::                                                           \
           [size] "n" (BITS / 8), [reg] "n" (REG),                      \
           [val] "n" ((u##BITS) VAL), [Lval] "i" (& Lval),              \
           [line] "n" (__LINE__), "r" (r##REG));                        \
  } while (0)

#define rtest_nowhile0(BITS, REG, VAL)                                  \
    PGM static const u##BITS Lval = VAL;                                \
    register u##BITS r##REG __asm (#REG) = VAL;                         \
    __asm ("run_test_gprs %[size], %[reg], %[val], %[Lval], %[line] "   \
           "; u" #BITS " r" #REG "=" #VAL ", line %[line];"             \
           ::                                                           \
           [size] "n" (BITS / 8), [reg] "n" (REG),                      \
           [val] "n" ((u##BITS) VAL), [Lval] "i" (& Lval),              \
           [line] "n" (__LINE__), "r" (r##REG))


#if ! __AVR_RODATA_IN_RAM__ || USE_VALUE
#define LOAD_INCZ "ld %0,%a1+"
#elif defined (__AVR_HAVE_LPMX__)
#define LOAD_INCZ "lpm %0,%a1+"
#else
#define LOAD_INCZ "lpm $ mov %0,r0 $ adiw r30,1"
#endif

// Called by .macro run_test_gprs which is invoked by rtest()
// resp. rtest_nowhile().
__attribute__((__used__))
void test_gprs (u8 n, u8 regno, const void *pval, int line)
{
  const u8 *r = gprs + regno;
  const u8 *pv = pval;
  for (u8 i = 0; i < n; ++i)
    {
      u8 vi;
      __asm (LOAD_INCZ : "=r" (vi), "+z" (pv));
      if (*r++ != vi)
        __builtin_exit (line);
    }
}
