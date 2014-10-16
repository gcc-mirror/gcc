/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef unsigned int UINT64 __attribute__((__mode__(__DI__)));

typedef struct
{
  UINT64 x[2] __attribute__((aligned(16)));
} fpreg;

struct ia64_args
{
  fpreg fp_regs[8];
  UINT64 gp_regs[8];
};

void
ffi_call(long i, long gpcount, long fpcount, void **avalue)
{
  struct ia64_args *stack;
  stack = __builtin_alloca (64);
  asm ("stf.spill %0 = %1%P0" : "=m" (*&stack->fp_regs[fpcount++])
                              : "f"(*(double *)avalue[i]));
  stack->gp_regs[gpcount++] = *(UINT64 *)avalue[i];
}
