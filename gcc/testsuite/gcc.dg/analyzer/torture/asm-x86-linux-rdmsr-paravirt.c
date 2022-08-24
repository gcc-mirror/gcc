/* Adapted from Linux: arch/x86/include/asm/paravirt.h */

/* { dg-do compile { target x86_64-pc-linux-gnu } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } { "" } } */
/* { dg-skip-if "" { *-*-* } { "-O0" } { "" } } */

/* Adapted/reduced from linux kernel (GPL-2.0).  */

#include "../analyzer-decls.h"

typedef unsigned char u8;
typedef unsigned __INT32_TYPE__ u32;
typedef unsigned __INT64_TYPE__ u64;
typedef __SIZE_TYPE__ size_t;

#define offsetof(TYPE, MEMBER)	((size_t)&((TYPE *)0)->MEMBER)

#define __stringify_1(x...)	#x
#define __stringify(x...)	__stringify_1(x)

# define __ASM_FORM(x, ...)		" " __stringify(x,##__VA_ARGS__) " "
# define __ASM_FORM_RAW(x, ...)		    __stringify(x,##__VA_ARGS__)

#ifndef __x86_64__
/* 32 bit */
# define __ASM_SEL(a,b)		__ASM_FORM(a)
# define __ASM_SEL_RAW(a,b)	__ASM_FORM_RAW(a)
#else
/* 64 bit */
# define __ASM_SEL(a,b)		__ASM_FORM(b)
# define __ASM_SEL_RAW(a,b)	__ASM_FORM_RAW(b)
#endif

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


/* snip */

/* ./arch/x86/include/asm/paravirt.h I think; was:
   PVOP_VCALL4(cpu.cpuid, eax, ebx, ecx, edx);

*/

#ifndef __x86_64__
#define CLBR_ANY  ((1 << 4) - 1)
#else
#define CLBR_ANY  ((1 << 9) - 1)
#endif /* X86_64 */

struct pv_cpu_ops {
  /* snip */
  u64 (*read_msr_safe)(unsigned int msr, int *err);
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

/*
 * Generate some code, and mark it as patchable by the
 * apply_paravirt() alternate instruction patcher.
 */
#define _paravirt_alt(insn_string, type, clobber)	\
	"771:\n\t" insn_string "\n" "772:\n"		\
	".pushsection .parainstructions,\"a\"\n"	\
	_ASM_ALIGN "\n"					\
	_ASM_PTR " 771b\n"				\
	"  .byte " type "\n"				\
	"  .byte 772b-771b\n"				\
	"  .short " clobber "\n"			\
	".popsection\n"

/* Generate patchable code, with the default asm parameters. */
#define paravirt_alt(insn_string)					\
	_paravirt_alt(insn_string, "%c[paravirt_typenum]", "%c[paravirt_clobber]")

#define PARAVIRT_CALL					\
	ANNOTATE_RETPOLINE_SAFE				\
	"call *%c[paravirt_opptr];"

#ifndef __x86_64__

/* 32-bit.  */

#define PVOP_CALL_ARGS							\
	unsigned long __eax = __eax, __edx = __edx, __ecx = __ecx;

#define PVOP_CALL_ARG1(x)		"a" ((unsigned long)(x))
#define PVOP_CALL_ARG2(x)		"d" ((unsigned long)(x))

#define PVOP_VCALL_CLOBBERS		"=a" (__eax), "=d" (__edx),	\
					"=c" (__ecx)
#define PVOP_CALL_CLOBBERS		PVOP_VCALL_CLOBBERS

#define PVOP_VCALLEE_CLOBBERS		"=a" (__eax), "=d" (__edx)
#define PVOP_CALLEE_CLOBBERS		PVOP_VCALLEE_CLOBBERS

#define EXTRA_CLOBBERS

#else

/* 64-bit.  */

/* [re]ax isn't an arg, but the return val */
#define PVOP_CALL_ARGS						\
	unsigned long __edi = __edi, __esi = __esi,		\
		__edx = __edx, __ecx = __ecx, __eax = __eax;

#define PVOP_CALL_ARG1(x)		"D" ((unsigned long)(x))
#define PVOP_CALL_ARG2(x)		"S" ((unsigned long)(x))

#define PVOP_VCALL_CLOBBERS	"=D" (__edi),				\
				"=S" (__esi), "=d" (__edx),		\
				"=c" (__ecx)
#define PVOP_CALL_CLOBBERS	PVOP_VCALL_CLOBBERS, "=a" (__eax)
#define PVOP_VCALLEE_CLOBBERS	"=a" (__eax)
#define PVOP_CALLEE_CLOBBERS	PVOP_VCALLEE_CLOBBERS

#define EXTRA_CLOBBERS	 , "r8", "r9", "r10", "r11"
#endif	/* CONFIG_X86_32 */

#define PVOP_TEST_NULL(op)	((void)pv_ops.op)

#define PVOP_RETVAL(rettype)						\
	({	unsigned long __mask = ~0UL;				\
		switch (sizeof(rettype)) {				\
		case 1: __mask =       0xffUL; break;			\
		case 2: __mask =     0xffffUL; break;			\
		case 4: __mask = 0xffffffffUL; break;			\
		default: break;						\
		}							\
		__mask & __eax;						\
	})

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

#define __PVOP_CALL(rettype, op, ...)					\
	____PVOP_CALL(PVOP_RETVAL(rettype), op, CLBR_ANY,		\
		      PVOP_CALL_CLOBBERS, EXTRA_CLOBBERS, ##__VA_ARGS__)

#define PVOP_CALL2(rettype, op, arg1, arg2)				\
	__PVOP_CALL(rettype, op, PVOP_CALL_ARG1(arg1), PVOP_CALL_ARG2(arg2))

static inline u64 paravirt_read_msr_safe(unsigned msr, int *err)
{
	return PVOP_CALL2(u64, cpu.read_msr_safe, msr, err);
}

#define rdmsr_safe(msr, a, b)				\
({							\
	int _err;					\
	u64 _l = paravirt_read_msr_safe(msr, &_err);	\
	(*a) = (u32)_l;					\
	(*b) = _l >> 32;				\
	_err;						\
})


void check_init_int(int);
void check_init_u32(u32);

void test(void)
{
  int err;
  u32 eax, edx;
  err = rdmsr_safe(0, &eax, &edx);
  check_init_int(err);
  check_init_u32(eax);
  check_init_u32(edx);
}
