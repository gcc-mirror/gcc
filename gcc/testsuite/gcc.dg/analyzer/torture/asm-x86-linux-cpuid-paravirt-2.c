/* { dg-do compile { target x86_64-pc-linux-gnu } } */
/* { dg-require-effective-target lp64 } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */

/* Adapted/reduced from linux kernel (GPL-2.0).  */

typedef __SIZE_TYPE__ size_t;

#define offsetof(TYPE, MEMBER)	((size_t)&((TYPE *)0)->MEMBER)

#define __stringify_1(x...)	#x
#define __stringify(x...)	__stringify_1(x)
#define __ASM_FORM(x, ...)		" " __stringify(x,##__VA_ARGS__) " "
#define __ASM_FORM_RAW(x, ...)		    __stringify(x,##__VA_ARGS__)
#define __ASM_SEL(a,b)		__ASM_FORM(b)
#define __ASM_SEL_RAW(a,b)	__ASM_FORM_RAW(b)
#define __ASM_REG(reg)         __ASM_SEL_RAW(e##reg, r##reg)
#define _ASM_PTR	__ASM_SEL(.long, .quad)
#define _ASM_ALIGN   __ASM_SEL(.balign 4, .balign 8)
#define _ASM_SP		__ASM_REG(sp)


register unsigned long current_stack_pointer asm(_ASM_SP);
#define ASM_CALL_CONSTRAINT "+r" (current_stack_pointer)

#define ANNOTATE_RETPOLINE_SAFE					\
	"999:\n\t"						\
	".pushsection .discard.retpoline_safe\n\t"		\
	_ASM_PTR " 999b\n\t"					\
	".popsection\n\t"

/* Adapted from Linux arch/x86/include/asm/paravirt.h  */

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

#define PARAVIRT_PATCH(x)					\
	(offsetof(struct paravirt_patch_template, x) / sizeof(void *))

#define paravirt_type(op)				\
	[paravirt_typenum] "i" (PARAVIRT_PATCH(op)),	\
	[paravirt_opptr] "i" (&(pv_ops.op))
#define paravirt_clobber(clobber)		\
	[paravirt_clobber] "i" (clobber)

#define CLBR_ANY  ((1 << 9) - 1)

#define _paravirt_alt(insn_string, type, clobber)	\
	"771:\n\t" insn_string "\n" "772:\n"		\
	".pushsection .parainstructions,\"a\"\n"	\
	_ASM_ALIGN "\n"					\
	_ASM_PTR " 771b\n"				\
	"  .byte " type "\n"				\
	"  .byte 772b-771b\n"				\
	"  .short " clobber "\n"			\
	".popsection\n"

#define paravirt_alt(insn_string)					\
	_paravirt_alt(insn_string, "%c[paravirt_typenum]", "%c[paravirt_clobber]")

#define PARAVIRT_CALL					\
	ANNOTATE_RETPOLINE_SAFE				\
	"call *%c[paravirt_opptr];"

#define PVOP_CALL_ARGS						\
	unsigned long __edi = __edi, __esi = __esi,		\
		__edx = __edx, __ecx = __ecx, __eax = __eax;

#define PVOP_CALL_ARG1(x)		"D" ((unsigned long)(x))
#define PVOP_CALL_ARG2(x)		"S" ((unsigned long)(x))
#define PVOP_CALL_ARG3(x)		"d" ((unsigned long)(x))
#define PVOP_CALL_ARG4(x)		"c" ((unsigned long)(x))

#define PVOP_VCALL_CLOBBERS	"=D" (__edi),				\
				"=S" (__esi), "=d" (__edx),		\
				"=c" (__ecx)
/* void functions are still allowed [re]ax for scratch */
#define PVOP_VCALLEE_CLOBBERS	"=a" (__eax)

#define VEXTRA_CLOBBERS	 , "rax", "r8", "r9", "r10", "r11"

#define PVOP_TEST_NULL(op)	((void)pv_ops.op)

#define ____PVOP_CALL(ret, op, clbr, call_clbr, extra_clbr, ...)	\
	({								\
		PVOP_CALL_ARGS;						\
		PVOP_TEST_NULL(op);					\
		asm volatile(paravirt_alt(PARAVIRT_CALL)		\
			     : call_clbr, ASM_CALL_CONSTRAINT		\
			     : paravirt_type(op),			\
			       paravirt_clobber(clbr),			\
			       ##__VA_ARGS__				\
			     : "memory", "cc" extra_clbr);		\
		ret;							\
	})

#define __PVOP_VCALL(op, ...)						\
	(void)____PVOP_CALL(, op, CLBR_ANY, PVOP_VCALL_CLOBBERS,	\
		       VEXTRA_CLOBBERS, ##__VA_ARGS__)

#define PVOP_VCALL4(op, arg1, arg2, arg3, arg4)				\
	__PVOP_VCALL(op, PVOP_CALL_ARG1(arg1), PVOP_CALL_ARG2(arg2),	\
		     PVOP_CALL_ARG3(arg3), PVOP_CALL_ARG4(arg4))

static void cpuid(unsigned int *eax, unsigned int *ebx, unsigned int *ecx,
		  unsigned int *edx)
{
  PVOP_VCALL4(cpu.cpuid, eax, ebx, ecx, edx);
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
