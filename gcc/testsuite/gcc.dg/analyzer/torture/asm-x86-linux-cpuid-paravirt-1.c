/* { dg-do assemble { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

/* Adapted/reduced from linux kernel (GPL-2.0).  */

register unsigned long current_stack_pointer asm("rsp");

struct pv_cpu_ops {
  /* snip */
  void (*cpuid)(unsigned int *eax, unsigned int *ebx, unsigned int *ecx,
                unsigned int *edx);
  /* snip */
};
struct paravirt_patch_template {
  struct pv_cpu_ops cpu;
  /* snip */
};
extern struct paravirt_patch_template pv_ops;

/* snip */
static void cpuid(unsigned int *eax, unsigned int *ebx, unsigned int *ecx,
		  unsigned int *edx) {
  unsigned long __edi = __edi, __esi = __esi, __edx = __edx, __ecx = __ecx,
                __eax = __eax;
  asm volatile(
      "771:\n\t"
      "999:\n\t"
      ".pushsection .discard.retpoline_safe\n\t"
      " "
      ".quad"
      " "
      " 999b\n\t"
      ".popsection\n\t"
      "call *%c[paravirt_opptr];"
      "\n"
      "772:\n"
      ".pushsection .parainstructions,\"a\"\n"
      " "
      ".balign 8"
      " "
      "\n"
      " "
      ".quad"
      " "
      " 771b\n"
      "  .byte "
      "%c[paravirt_typenum]"
      "\n"
      "  .byte 772b-771b\n"
      "  .short "
      "%c[paravirt_clobber]"
      "\n"
      ".popsection\n"
      : "=D"(__edi), "=S"(__esi), "=d"(__edx), "=c"(__ecx),
        "+r"(current_stack_pointer)
      : [ paravirt_typenum ] "i"(
            (__builtin_offsetof(struct paravirt_patch_template, cpu.cpuid) /
             sizeof(void *))),
        [ paravirt_opptr ] "i"(&(pv_ops.cpu.cpuid)),
        [ paravirt_clobber ] "i"(((1 << 9) - 1)), "D"((unsigned long)(eax)),
        "S"((unsigned long)(ebx)), "d"((unsigned long)(ecx)),
        "c"((unsigned long)(edx))
      : "memory", "cc", "rax", "r8", "r9", "r10", "r11");
}

extern void check_init_int(int v);

void test(unsigned int op) {
  unsigned int eax, ebx, ecx, edx;

  eax = op;
  ecx = 0;
  cpuid(&eax, &ebx, &ecx, &edx);

  check_init_int(eax);
  check_init_int(ebx); /* { dg-bogus "use of uninitialized value 'ebx'" } */
  check_init_int(ecx);
  check_init_int(edx); /* { dg-bogus "use of uninitialized value 'edx'" } */
}
