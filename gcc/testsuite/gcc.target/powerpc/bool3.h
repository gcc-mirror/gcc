/* Test forcing 128-bit logical types into GPR registers.  */

#if defined(NO_ASM)
#define FORCE_REG1(X)
#define FORCE_REG2(X,Y)

#else
#if defined(USE_ALTIVEC)
#define REG_CLASS "+v"
#define PRINT_REG1 "# altivec reg %0"
#define PRINT_REG2 "# altivec reg %0, %1"

#elif defined(USE_FPR)
#define REG_CLASS "+d"
#define PRINT_REG1 "# fpr reg %0"
#define PRINT_REG2 "# fpr reg %0, %1"

#elif defined(USE_VSX)
#define REG_CLASS "+wa"
#define PRINT_REG1 "# vsx reg %x0"
#define PRINT_REG2 "# vsx reg %x0, %x1"

#else
#define REG_CLASS "+r"
#define PRINT_REG1 "# gpr reg %0"
#define PRINT_REG2 "# gpr reg %0, %1"
#endif

#define FORCE_REG1(X) __asm__ (PRINT_REG1 : REG_CLASS (X))
#define FORCE_REG2(X,Y) __asm__ (PRINT_REG2 : REG_CLASS (X), REG_CLASS (Y))
#endif

void ptr1 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = a & b;					/* AND */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr2 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = a | b;					/* OR */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr3 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = a ^ b;					/* XOR */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr4 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b;

  FORCE_REG1 (a);
  b = ~a;					/* NOR */
  FORCE_REG1 (b);
  p[0] = b;
}

void ptr5 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = ~(a & b);					   /* NAND */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr6 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = ~(a | b);					   /* AND */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr7 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = ~(a ^ b);					   /* EQV */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr8 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = (~a) & b;					   /* ANDC */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr9 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = (~a) | b;					   /* ORC */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr10 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = (~a) ^ b;					   /* EQV */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr11 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = a & (~b);					   /* ANDC */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr12 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = a | (~b);					   /* ORC */
  FORCE_REG1 (c);
  p[0] = c;
}

void ptr13 (TYPE *p)
{
  TYPE a = p[1];
  TYPE b = p[2];
  TYPE c;

  FORCE_REG2 (a, b);
  c = a ^ (~b);					   /* AND */
  FORCE_REG1 (c);
  p[0] = c;
}
