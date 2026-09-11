/* PR rtl-optimization/127244 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */

typedef unsigned int __u32;

extern unsigned long __rdgsbase_inactive (void);

static inline unsigned long
native_save_fl (void) {
  unsigned long flags;
  asm volatile ("# __raw_save_flags\n\tpushf ; pop %0"
		: "=rm" (flags)
		:
		: "memory");
  return flags;
}

static inline void
native_irq_disable (void) {
  asm volatile ("cli" : : : "memory");
}

static inline void
native_irq_enable (void) {
  asm volatile ("sti" : : : "memory");
}

struct cpuinfo_x86 {
  union { __u32 x86_capability[24]; };
};

struct cpuinfo_x86 boot_cpu_data;

static inline _Bool
constant_test_bit (long nr, const volatile unsigned long *addr) {
  return ((1UL << (nr & 63)) & addr[nr >> 6]) != 0;
}

unsigned long
x86_gsbase_read_cpu_inactive (void) {
  unsigned long gsbase;

  if (constant_test_bit (9 * 32, (unsigned long *) boot_cpu_data.x86_capability)) {
    unsigned long flags = native_save_fl ();
    native_irq_disable ();
    gsbase = __rdgsbase_inactive ();
    if (flags & (1UL << 9))
      native_irq_enable ();
  }

  return gsbase;
}

/* { dg-final { scan-assembler "pushf ; pop %r" } } */
/* { dg-final { scan-assembler-not "pushf ; pop \[0-9-]*\\(%rsp\\)" } } */
/* { dg-final { scan-assembler-not "movq\[ \t\]+\[0-9-]*\\(%rsp\\), %r" } } */
