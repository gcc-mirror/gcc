/* PR tree-optimization/100834 */
/* { dg-do compile { target size32plus } } */
/* { dg-options "-O2 -Wall" } */

#define PAGE_SIZE 	4096
#define STACK_SIZE 	PAGE_SIZE

union registers
{
  struct
  {
    unsigned long r15, r14, r13, r12, r11, r10, r9, r8;
    unsigned long rdi, rsi, rbp, unused, rbx, rdx, rcx, rax;
  };
  unsigned long by_index[16];
};

struct per_cpu
{
  union
  {
    unsigned char stack[STACK_SIZE];
    struct
    {
      unsigned char __fill[STACK_SIZE - sizeof (union registers)];
      union registers guest_regs;
    };
  };
} __attribute__((aligned (PAGE_SIZE)));

static inline struct per_cpu *
this_cpu_data (void)
{
  return (struct per_cpu *) 0xdeadbeef;
}

void
foo (void)
{
  struct per_cpu *cpu_data = this_cpu_data ();
  __builtin_memset (&cpu_data->guest_regs, 0, sizeof (cpu_data->guest_regs));	/* { dg-bogus "is out of the bounds" } */
}
