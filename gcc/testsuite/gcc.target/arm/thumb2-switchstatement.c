/* { dg-do compile } */
/* { dg-options "-mthumb --param case-values-threshold=1 -fno-reorder-blocks -fno-tree-dce -O2" } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#define NOP "nop;"
#define NOP2 NOP NOP
#define NOP4 NOP2 NOP2
#define NOP8 NOP4 NOP4
#define NOP16 NOP8 NOP8
#define NOP32 NOP16 NOP16
#define NOP64 NOP32 NOP32
#define NOP128 NOP64 NOP64
#define NOP256 NOP128 NOP128
#define NOP512 NOP256 NOP256
#define NOP1024 NOP512 NOP512
#define NOP2048 NOP1024 NOP1024
#define NOP4096 NOP2048 NOP2048
#define NOP8192 NOP4096 NOP4096
#define NOP16384 NOP8192 NOP8192
#define NOP32768 NOP16384 NOP16384
#define NOP65536 NOP32768 NOP32768
#define NOP131072 NOP65536 NOP65536

enum z
{
  a = 1,
  b,
  c,
  d,
  e,
  f = 7,
};

inline void QIFunction (const char* flag)
{
  asm volatile (NOP32);
  return;
}

inline void HIFunction (const char* flag)
{
  asm volatile (NOP512);
  return;
}

inline void SIFunction (const char* flag)
{
  asm volatile (NOP131072);
  return;
}

/*
**QImode_test:
**	...
**	tbb	\[pc, r[0-9]+\]
**	...
*/
__attribute__ ((noinline)) __attribute__ ((noclone)) const char* QImode_test(enum z x)
{
  switch (x)
    {
      case d:
        QIFunction("QItest");
        return "InlineASM";
      case f:
        return "TEST";
      default:
        return "Default";
    }
}

/* { dg-final { scan-assembler ".byte" } } */

/*
**HImode_test:
**	...
**	tbh	\[pc, r[0-9]+, lsl #1\]
**	...
*/
__attribute__ ((noinline)) __attribute__ ((noclone)) const char* HImode_test(enum z x)
{
  switch (x)
  {
    case d:
      HIFunction("HItest");
      return "InlineASM";
    case f:
      return "TEST";
    default:
      return "Default";
  }
}

/* { dg-final { scan-assembler ".2byte" } } */

/*
**SImode_test:
**	...
**	adr	(r[0-9]+), .L[0-9]+
**	ldr	pc, \[\1, r[0-9]+, lsl #2\]
**	...
*/
__attribute__ ((noinline)) __attribute__ ((noclone)) const char* SImode_test(enum z x)
{
  switch (x)
  {
    case d:
      SIFunction("SItest");
      return "InlineASM";
    case f:
      return "TEST";
    default:
      return "Default";
  }
}

/* { dg-final { scan-assembler ".word" } } */

/*
**backwards_branch_test:
**	...
**	adr	(r[0-9]+), .L[0-9]+
**	ldr	pc, \[\1, r[0-9]+, lsl #2\]
**	...
*/
__attribute__ ((noinline)) __attribute__ ((noclone)) const char* backwards_branch_test(enum z x, int flag)
{
  if (flag == 5)
  {
    backwards:
      asm volatile (NOP512);
      return "ASM";
  }
  switch (x)
  {
    case d:
      goto backwards;
    case f:
      return "TEST";
    default:
      return "Default";
  }
}
