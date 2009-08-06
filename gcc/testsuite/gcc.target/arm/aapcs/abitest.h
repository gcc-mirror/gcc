#define IN_FRAMEWORK

#ifdef VFP
#define D0	0
#define D1	8
#define D2	16
#define D3	24
#define D4	32
#define D5	40
#define D6	48
#define D7	56

#define S0	64
#define S1	68
#define S2	72
#define S3	76
#define S4	80
#define S5	84
#define S6	88
#define S7	92
#define S8      86
#define S9	100
#define S10	104
#define S11	108
#define S12	112
#define S13	116
#define S14	120
#define S15	124

#define R0	128
#define R1	132
#define R2	136
#define R3	140

#define STACK	144

#else

#define R0	0
#define R1	4
#define R2	8
#define R3	12

#define STACK   16

#endif

extern void abort (void);

__attribute__((naked))  void dumpregs () __asm("myfunc");
__attribute__((naked))  void dumpregs ()
{
  asm(
      "mov	ip, sp\n\t"
      "stmfd	sp!, {r0-r3}\n\t"
#ifdef VFP
      "fstmdbs	sp!, {s0-s15}\n\t"
      "fstmdbd	sp!, {d0-d7}\n\t"
#endif
      "mov	r0, sp\n\t"
      "stmfd	sp!, {ip, r14}\n\t"
      "bl	testfunc\n\t"
      "ldmfd	sp!, {r0, r14}\n\t"
      "mov	sp, r0\n\t"
      "bx	lr");
}


#define LAST_ARG(type,val,offset) { type __x = val; if (memcmp(&__x, stack+offset, sizeof(type)) != 0) abort(); }
#define ARG(type,val,offset) LAST_ARG(type, val, offset)
#define ANON(type,val,offset) LAST_ARG(type, val, offset)
#define LAST_ANON(type,val,offset) LAST_ARG(type, val, offset)
#define DOTS

void testfunc(char* stack)
{
#include TESTFILE
  return;
}

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#define LAST_ARG(type,val,offset) type
#define ARG(type,val,offset) LAST_ARG(type, val, offset),
#define DOTS ...
#define ANON(type,val, offset)
#define LAST_ANON(type,val, offset)

#ifndef MYFUNCTYPE
#define MYFUNCTYPE void
#endif

#ifndef PCSATTR
#define PCSATTR
#endif

MYFUNCTYPE myfunc(
#include TESTFILE
) PCSATTR;

#undef LAST_ARG
#undef ARG
#undef DOTS
#undef ANON
#undef LAST_ANON
#define LAST_ARG(type,val,offset) val
#define ARG(type,val,offset) LAST_ARG(type, val, offset),
#define DOTS
#define LAST_ANON(type,val,offset) LAST_ARG(type, val, offset)
#define ANON(type,val,offset) LAST_ARG(type, val, offset),


int main()
{
  myfunc(
#include TESTFILE
);
  return 0;
}
